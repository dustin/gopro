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
	id        string
	gpf       File
	cacheFile string
	parent    *goProMedium

	// Stuff wants to grab the end, so let's prepare it
	have []blockState
	mu   sync.Mutex
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

func (gf *goProFile) initBlocks(p string) {
	gf.have = make([]blockState, 1+int(blockCount(uint64(gf.gpf.Size))))
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
		gf.have[i] = ready
	}
}

func (gf *goProFile) Open(ctx context.Context, flags uint32) (fh fs.FileHandle, flg uint32, errno syscall.Errno) {
	if ignore(ctx) {
		log.Printf("Ignoring open call")
		return nil, 0, syscall.EPERM
	}
	gf.mu.Lock()
	defer gf.mu.Unlock()

	r := gf.Root()
	gr, ok := gf.Root().Operations().(*GoProRoot)
	if !ok {
		log.Printf("Root isn't GoProRoot, it's %v", r)
		return nil, 0, syscall.EIO
	}
	gfh := goProFileHandle{procName: procName(ctx)}

	defer func() {
		if errno == 0 {
			gr.trackOpen(gf.gpf, gfh.procName)
			log.Printf("%v opened %v", fh.(goProFileHandle).procName, gf.gpf.Name)
		}
	}()

	myPath := fmt.Sprintf("%v/%02d/%v/%v", gf.parent.captured.Year(), gf.parent.captured.Month(), gf.parent.id, gf.gpf.Name)
	caches := append(gr.sources, gr.cacheDir)

	for _, root := range caches {
		p := filepath.Join(root, myPath)

		log.Printf("Trying %v", p)
		f, err := os.OpenFile(p, os.O_RDONLY, 0444)
		if err == nil {
			log.Printf(" -- found %v", p)
			gfh.file = f
			return gfh, fuse.FOPEN_KEEP_CACHE, 0
		}
		if os.IsNotExist(err) {
			continue
		}
		log.Printf("Failed to open %v: %v\n", p, err)
		return nil, 0, syscall.EIO
	}

	log.Printf("Not found locally.  Let's do some cloud streaming stuff.")

	p := filepath.Join(gr.cacheDir, myPath)
	err := os.MkdirAll(filepath.Dir(p), 0777)
	if err != nil {
		return nil, 0, syscall.EIO
	}

	f, err := os.OpenFile(p+".tmp", os.O_RDWR|os.O_CREATE, 0666)
	if err != nil {
		return nil, 0, syscall.EIO
	}
	gf.cacheFile = p
	gf.initBlocks(p)

	gfh.file = f
	return gfh, fuse.FOPEN_KEEP_CACHE, 0
}

func (gf *goProFile) startFetch(f *os.File, block uint64) error {
	// This is called with a lock held.
	u, err := gf.parent.origURL(gf.gpf)
	if err != nil {
		return err
	}
	log.Printf("Fetching block %v for %v", block, gf.gpf.Name)
	l, h := blockRange(block)
	gf.have[block] = fetching
	_, err = fillHole(f, u, l, h)
	if err != nil {
		log.Printf("Failed to fetch block %v: %v", block, err)
		gf.have[int(block)] = missing
	} else {
		gf.have[int(block)] = ready
	}
	return err
}

func (gf *goProFile) waitForBlocks(f *os.File, blocks []uint64) error {
	gf.mu.Lock()
	defer gf.mu.Unlock()

	if gf.have == nil {
		return nil
	}

	for {
		allDone := true
		for _, b := range blocks {
			switch gf.have[int(b)] {
			case missing:
				if err := gf.startFetch(f, b); err != nil {
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

func (gf *goProFile) Read(ctx context.Context, fh fs.FileHandle, dest []byte, off int64) (fuse.ReadResult, syscall.Errno) {
	if off >= int64(gf.gpf.Size) {
		return fuse.ReadResultData(nil), fs.ToErrno(io.EOF)
	}

	gfh := fh.(goProFileHandle)

	blocks := []uint64{blockCount(uint64(off))}
	for i := blocks[0] + 1; i < blockCount(uint64(off+int64(len(dest)))); i++ {
		blocks = append(blocks, i)
	}

	if err := gf.waitForBlocks(gfh.file, blocks); err != nil {
		return fuse.ReadResultData(nil), syscall.EIO
	}

	return fuse.ReadResultFd(gfh.file.Fd(), off, len(dest)), 0
}

type goProFileHandle struct {
	procName string
	file     *os.File
}

func (gf *goProFile) Getxattr(ctx context.Context, attr string, dest []byte) (uint32, syscall.Errno) {
	log.Printf("Attempting to get xattributes for %v: %v", gf.gpf.Name, attr)
	return 0, syscall.ENOATTR
}

func (gf *goProFile) Setxattr(ctx context.Context, attr string, data []byte, flags uint32) syscall.Errno {
	log.Printf("Attempting to set xattr %v: %v", gf.gpf.Name, attr)
	return syscall.ENOATTR
}

func (gf *goProFile) Release(ctx context.Context, fh fs.FileHandle) syscall.Errno {
	gf.mu.Lock()
	defer gf.mu.Unlock()

	gfh := fh.(goProFileHandle)
	gfh.file.Close()

	r := gf.Root()
	gr, ok := gf.Root().Operations().(*GoProRoot)
	if !ok {
		log.Printf("Root isn't GoProRoot, it's %v", r)
	} else {
		gr.trackClose(gf.gpf, fh.(goProFileHandle).procName)
		log.Printf("%v closed %v", fh.(goProFileHandle).procName, gf.gpf.Name)
	}

	if gf.cacheFile != "" {
		allDone := true
		for i := uint64(0); i <= blockCount(uint64(gf.gpf.Size)); i++ {
			allDone = allDone && gf.have[i] == ready
		}
		if allDone {
			log.Printf("All blocks are ready for %v, moving file into place.", gf.gpf.Name)
			if err := os.Rename(gf.cacheFile+".tmp", gf.cacheFile); err != nil {
				log.Printf("Error renaming file:  %v", err)
			}
			gf.cacheFile = ""
		} else {
			completed := []int{}
			for i, h := range gf.have {
				if h == ready {
					completed = append(completed, i)
				}
			}
			j, err := json.Marshal(completed)
			if err != nil {
				log.Printf("Error marshaling blocks: %v", err)
				return 0
			}
			if err := os.WriteFile(gf.cacheFile+".blocks", j, 0666); err != nil {
				log.Printf("Error writing block file: %v", err)
				return 0
			}
		}
	}
	return 0
}

var _ = (fs.NodeOpener)((*goProFile)(nil))
var _ = (fs.NodeGetattrer)((*goProFile)(nil))
var _ = (fs.NodeReader)((*goProFile)(nil))
var _ = (fs.NodeReleaser)((*goProFile)(nil))
var _ = (fs.NodeGetattrer)((*goProFile)(nil))
var _ = (fs.NodeGetxattrer)((*goProFile)(nil))
var _ = (fs.NodeSetxattrer)((*goProFile)(nil))
