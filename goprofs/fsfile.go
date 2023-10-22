package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"sync"

	"syscall"

	"github.com/hanwen/go-fuse/v2/fs"
	"github.com/hanwen/go-fuse/v2/fuse"
)

type blockState int

const (
	missing = iota
	fetching
	ready
)

type goProFile struct {
	fs.Inode
	id     string
	gpf    File
	parent *goProMedium

	mu sync.Mutex
}

func (gf *goProFile) Getattr(ctx context.Context, f fs.FileHandle, out *fuse.AttrOut) syscall.Errno {
	out.Mode = 0444
	out.Nlink = 1
	out.Mtime = uint64(gf.parent.captured.Unix())
	out.Atime = out.Mtime
	out.Ctime = out.Mtime
	out.Size = gf.gpf.Size
	const bs = 512
	out.Blksize = bs
	out.Blocks = (out.Size + bs - 1) / bs
	return 0
}

func (gf *goProFile) Open(ctx context.Context, flags uint32) (fh fs.FileHandle, flg uint32, errno syscall.Errno) {
	if ignore(ctx) {
		log.Printf("Ignoring open call")
		return nil, 0, syscall.EPERM
	}

	r := gf.Root()
	gr, ok := gf.Root().Operations().(*GoProRoot)
	if !ok {
		log.Printf("Root isn't GoProRoot, it's %v", r)
		return nil, 0, syscall.EIO
	}
	caller := procName(ctx)

	defer func() {
		if errno == 0 {
			if t, ok := fh.(disposalTracker); ok {
				t.addCallback(gr.trackOpen(gf.gpf, caller))
			}
		}
	}()

	fh, flg, errno = gf.openExisting(ctx, flags, gr)
	if errno == 0 {
		return fh, flg, errno
	}
	return gf.openOrigin(ctx, flags, gr)
}

func (gf *goProFile) myPath() string {
	return fmt.Sprintf("%v/%02d/%v/%v", gf.parent.captured.Year(), gf.parent.captured.Month(), gf.parent.id, gf.gpf.Name)
}

func (gf *goProFile) openExisting(ctx context.Context, flags uint32, gr *GoProRoot) (fh fs.FileHandle, flg uint32, errno syscall.Errno) {
	caches := append(gr.sources, gr.cacheDir)

	for _, root := range caches {
		p := filepath.Join(root, gf.myPath())

		f, err := os.OpenFile(p, os.O_RDONLY, 0444)
		if err == nil {
			log.Printf("Found %v", p)
			return &goProLoopHandle{file: f, onClose: func() { log.Printf("existing closed") }}, fuse.FOPEN_KEEP_CACHE, 0
		}
		if os.IsNotExist(err) {
			continue
		}
		log.Printf("Failed to open %v: %v\n", p, err)
		return nil, 0, syscall.EIO
	}

	return nil, 0, syscall.ENOENT
}

func (gf *goProFile) openOrigin(ctx context.Context, flags uint32, gr *GoProRoot) (fh fs.FileHandle, flg uint32, errno syscall.Errno) {
	log.Printf("%v not found locally.  Let's do some cloud streaming stuff.", gf.myPath())

	gfh := &goProFileHandle{size: gf.gpf.Size, med: gf.parent, gpf: gf.gpf}
	p := filepath.Join(gr.cacheDir, gf.myPath())
	err := os.MkdirAll(filepath.Dir(p), 0777)
	if err != nil {
		return nil, 0, syscall.EIO
	}

	f, err := os.OpenFile(p+".tmp", os.O_RDWR|os.O_CREATE, 0666)
	if err != nil {
		return nil, 0, syscall.EIO
	}
	gfh.cacheFile = p
	gfh.initBlocks(p)

	gfh.file = f
	return gfh, fuse.FOPEN_KEEP_CACHE, 0
}

func (gfh *goProFileHandle) startFetch(f *os.File, block uint64) error {
	// This is called with a lock held.
	u, err := gfh.med.origURL(gfh.gpf)
	if err != nil {
		return err
	}
	log.Printf("Fetching block %v for %v", block, gfh.gpf.Name)
	l, h := blockRange(block)
	gfh.have[block] = fetching
	_, err = fillHole(f, u, l, h)
	if err != nil {
		log.Printf("Failed to fetch block %v: %v", block, err)
		gfh.have[int(block)] = missing
	} else {
		gfh.have[int(block)] = ready
	}
	return err
}

func (gfh *goProFileHandle) waitForBlocks(f *os.File, blocks []uint64) error {
	gfh.mu.Lock()
	defer gfh.mu.Unlock()

	if gfh.have == nil {
		return nil
	}

	for {
		allDone := true
		for _, b := range blocks {
			switch gfh.have[int(b)] {
			case missing:
				if err := gfh.startFetch(f, b); err != nil {
					log.Printf("Failed starting a fetch: %v", err)
					return err
				}
				allDone = false
			case fetching:
				allDone = false
			case ready:
			}
		}
		if allDone {
			return nil
		}
	}
}

func (gfh *goProFileHandle) Read(ctx context.Context, dest []byte, off int64) (fuse.ReadResult, syscall.Errno) {
	blocks := []uint64{blockCount(uint64(off))}
	for i := blocks[0] + 1; i < blockCount(uint64(off+int64(len(dest)))); i++ {
		blocks = append(blocks, i)
	}

	if err := gfh.waitForBlocks(gfh.file, blocks); err != nil {
		return fuse.ReadResultData(nil), syscall.EIO
	}

	return fuse.ReadResultFd(gfh.file.Fd(), off, len(dest)), 0
}

func (gf *goProFile) Read(ctx context.Context, fh fs.FileHandle, dest []byte, off int64) (fuse.ReadResult, syscall.Errno) {
	if off >= int64(gf.gpf.Size) {
		return fuse.ReadResultData(nil), fs.ToErrno(io.EOF)
	}

	return fh.(fs.FileReader).Read(ctx, dest, off)
}

type goProLoopHandle struct {
	onClose func()
	file    *os.File
}

func (h *goProLoopHandle) Release(ctx context.Context) syscall.Errno {
	h.onClose()
	return fs.ToErrno(h.file.Close())
}

func (h *goProLoopHandle) Read(ctx context.Context, dest []byte, off int64) (fuse.ReadResult, syscall.Errno) {
	return fuse.ReadResultFd(h.file.Fd(), off, len(dest)), 0
}

func (h *goProLoopHandle) addCallback(f func()) {
	h.onClose = f
}

type disposalTracker interface {
	addCallback(func())
}

var _ = (fs.FileReleaser)((*goProLoopHandle)(nil))
var _ = (fs.FileReader)((*goProLoopHandle)(nil))
var _ = (disposalTracker)((*goProLoopHandle)(nil))
var _ = (fs.FileReleaser)((*goProFileHandle)(nil))
var _ = (fs.FileReader)((*goProFileHandle)(nil))
var _ = (disposalTracker)((*goProFileHandle)(nil))

type goProFileHandle struct {
	gpf       File
	med       *goProMedium
	file      *os.File
	size      uint64
	have      []blockState
	cacheFile string
	onClose   func()
	mu        sync.Mutex
}

func (h *goProFileHandle) addCallback(f func()) {
	h.onClose = f
}

func (gfh *goProFileHandle) initBlocks(p string) {
	gfh.have = make([]blockState, 1+int(blockCount(gfh.size)))
	f, err := os.Open(p + ".blocks")
	if err != nil {
		return
	}
	defer f.Close()
	nums := []int{}
	if err := json.NewDecoder(f).Decode(&nums); err != nil {
		return
	}
	for _, i := range nums {
		gfh.have[i] = ready
	}
}

func (gf *goProFile) Getxattr(ctx context.Context, attr string, dest []byte) (uint32, syscall.Errno) {
	log.Printf("Attempting to get xattributes for %v: %v", gf.gpf.Name, attr)
	return 0, syscall.ENOATTR
}

func (gf *goProFile) Setxattr(ctx context.Context, attr string, data []byte, flags uint32) syscall.Errno {
	log.Printf("Attempting to set xattr %v: %v", gf.gpf.Name, attr)
	return syscall.ENOATTR
}

func (gfh *goProFileHandle) Release(ctx context.Context) syscall.Errno {
	gfh.mu.Lock()
	defer gfh.mu.Unlock()

	allDone := true
	for i := uint64(0); i <= blockCount(uint64(gfh.size)); i++ {
		allDone = allDone && gfh.have[i] == ready
	}
	if allDone {
		log.Printf("All blocks are ready for %v, moving file into place.", gfh.gpf.Name)
		if err := os.Rename(gfh.cacheFile+".tmp", gfh.cacheFile); err != nil {
			log.Printf("Error renaming file:  %v", err)
		}
	} else {
		completed := []int{}
		for i, h := range gfh.have {
			if h == ready {
				completed = append(completed, i)
			}
		}
		j, err := json.Marshal(completed)
		if err != nil {
			log.Printf("Error marshaling blocks: %v", err)
			return 0
		}
		if err := os.WriteFile(gfh.cacheFile+".blocks", j, 0666); err != nil {
			log.Printf("Error writing block file: %v", err)
		}
	}
	return 0
}

func (gf *goProFile) Release(ctx context.Context, fh fs.FileHandle) syscall.Errno {
	return fh.(fs.FileReleaser).Release(ctx)
}

var _ = (fs.NodeOpener)((*goProFile)(nil))
var _ = (fs.NodeGetattrer)((*goProFile)(nil))
var _ = (fs.NodeReader)((*goProFile)(nil))
var _ = (fs.NodeReleaser)((*goProFile)(nil))
var _ = (fs.NodeGetattrer)((*goProFile)(nil))
var _ = (fs.NodeGetxattrer)((*goProFile)(nil))
var _ = (fs.NodeSetxattrer)((*goProFile)(nil))
