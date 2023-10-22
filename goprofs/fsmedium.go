package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"sort"
	"sync"
	"time"

	"syscall"

	"github.com/hanwen/go-fuse/v2/fs"
	"github.com/hanwen/go-fuse/v2/fuse"
)

type goProMedium struct {
	fs.Inode
	id       string
	captured time.Time

	origUrls map[File]string
	mu       sync.Mutex
}

func (gm *goProMedium) proxyDir() (string, string, error) {
	r := gm.Root()
	gr, ok := gm.Root().Operations().(*GoProRoot)
	if !ok {
		return "", "", fmt.Errorf("root isn't GoProRoot, it's %v", r)
	}
	if gr.proxyDir == "" {
		return "", "", fmt.Errorf("proxy dir isn't configured")
	}

	return gr.proxyDir, filepath.Join(gr.proxyDir, fmt.Sprintf("%v/%02d/%v/Proxy", gm.captured.Year(), gm.captured.Month(), gm.id)), nil
}

func (gm *goProMedium) proxyExists() bool {
	_, d, err := gm.proxyDir()
	if err != nil {
		return false
	}
	if stat, err := os.Stat(d); err == nil && stat.IsDir() {
		return true
	}
	return false
}

func (gm *goProMedium) origURL(f File) (string, error) {
	gm.mu.Lock()
	defer gm.mu.Unlock()
	if gm.origUrls == nil {
		r := gm.Root()
		gr, ok := r.Operations().(*GoProRoot)
		if !ok {
			log.Printf("Root isn't GoProRoot, it's %v", r)
			return "", fmt.Errorf("root isn't GoProRoot: %v", r)
		}
		log.Printf("Fetching origin URLs for %v", gm.id)
		m, err := fetchURLs(gr.baseURL, gm.id)
		if err != nil {
			log.Printf("failed to get URLs for %v: %v", gm.id, err)
			return "", err
		}
		gm.origUrls = m
		time.AfterFunc(1800*time.Second, func() {
			gm.mu.Lock()
			defer gm.mu.Unlock()
			gm.origUrls = nil
		})
	}

	f.Name = ""
	u, ok := gm.origUrls[f]
	if !ok {
		keys := []string{}
		for k := range gm.origUrls {
			keys = append(keys, fmt.Sprintf("%#v", k))
		}
		return "", fmt.Errorf("Can't find %v in %v", f, keys)
	}
	return u, nil
}

func (gm *goProMedium) Unlink(ctx context.Context, name string) syscall.Errno {
	if name != ".Proxy.lock" {
		fmt.Printf("Trying to unlink %v", name)
		return syscall.EPERM
	}
	gm.RmChild(name)
	return 0
}

func (gm *goProMedium) Create(ctx context.Context, name string, flags uint32, mode uint32, out *fuse.EntryOut) (node *fs.Inode, fh fs.FileHandle, fuseFlags uint32, errno syscall.Errno) {
	if name == ".Proxy.lock" {
		log.Printf("Creating .Proxy.lock")
		gm.mu.Lock()
		defer gm.mu.Unlock()

		if ch := gm.GetChild(".Proxy.lock"); ch != nil {
			return ch, nil, 0, 0
		}

		ch := gm.NewPersistentInode(ctx, &fs.MemRegularFile{}, fs.StableAttr{})
		gm.AddChild(name, ch, true)
		return ch, nil, fuse.FOPEN_KEEP_CACHE, 0
	}
	log.Printf("Attempting to create %v", name)
	return nil, nil, 0, syscall.EROFS
}

func (gm *goProMedium) wasMissing(name string) {
	r := gm.Root()
	gr, ok := gm.Root().Operations().(*GoProRoot)
	if !ok {
		log.Printf("root isn't GoRoot while remembering missing: %v", r)
		return
	}
	gr.wasMissing(fmt.Sprintf("%v/%02d/%v/%v", gm.captured.Year(), gm.captured.Month(), gm.id, name))
}

func (gm *goProMedium) Lookup(ctx context.Context, name string, out *fuse.EntryOut) (*fs.Inode, syscall.Errno) {
	if ch := gm.GetChild(name); ch != nil {
		return ch, 0
	}

	if name == "Proxy" && gm.proxyExists() {
		prox, err := gm.createProxy(ctx)
		if err == nil {
			return prox, 0
		} else {
			log.Printf("Error creating proxy: %v", err)
		}
	}
	gm.wasMissing(name)
	return nil, syscall.ENOENT
}

func (gm *goProMedium) Readdir(ctx context.Context) (fs.DirStream, syscall.Errno) {
	rv := []fuse.DirEntry{}

	for n, i := range gm.Children() {
		sa := i.StableAttr()
		rv = append(rv, fuse.DirEntry{Mode: sa.Mode, Name: n, Ino: sa.Ino})
	}

	if gm.GetChild("Proxy") == nil && gm.proxyExists() {
		if prox, err := gm.createProxy(ctx); err != nil {
			log.Printf("Error creating proxy: %v", err)
		} else {
			sa := prox.StableAttr()
			rv = append(rv, fuse.DirEntry{Mode: sa.Mode, Name: "Proxy", Ino: sa.Ino})
		}
	}

	sort.Slice(rv, func(i, j int) bool {
		return rv[i].Name < rv[j].Name
	})

	return fs.NewListDirStream(rv), 0
}

func (gm *goProMedium) createProxy(ctx context.Context) (*fs.Inode, error) {
	gm.mu.Lock()
	defer gm.mu.Unlock()

	if ch := gm.GetChild("Proxy"); ch != nil {
		return ch, nil
	}

	proot, pdir, err := gm.proxyDir()
	if err != nil {
		return nil, fmt.Errorf("Error getting proxy dir: %v", err)
	}
	if err := os.MkdirAll(pdir, 0777); err != nil {
		return nil, fmt.Errorf("Error creating %v: %v", pdir, err)
	}
	loop, err := fs.NewLoopbackRoot(proot)
	if err != nil {
		return nil, fmt.Errorf("Error creating loopback: %v", err)
	}
	ch := gm.NewPersistentInode(ctx, loop, fs.StableAttr{Mode: fuse.S_IFDIR})
	gm.AddChild("Proxy", ch, true)
	return ch, nil
}

func (gm *goProMedium) Mkdir(ctx context.Context, name string, mode uint32, out *fuse.EntryOut) (*fs.Inode, syscall.Errno) {
	if name != "Proxy" {
		log.Printf("Trying to create a non-proxy directory:  %v", name)
		return nil, syscall.EROFS
	}
	prox, err := gm.createProxy(ctx)
	if err != nil {
		log.Printf("Error creating proxy directory for %v: %v", gm.id, err)
		return nil, syscall.EROFS
	}
	return prox, 0
}

func (gm *goProMedium) Getattr(ctx context.Context, f fs.FileHandle, out *fuse.AttrOut) syscall.Errno {
	out.Mode = 0777
	out.Nlink = 1
	out.Mtime = uint64(gm.captured.Unix())
	out.Atime = out.Mtime
	out.Ctime = out.Mtime
	out.Size = 0
	// Do we link children?
	const bs = 512
	out.Blksize = bs
	out.Blocks = (out.Size + bs - 1) / bs
	return 0
}

var _ = (fs.NodeGetattrer)((*goProMedium)(nil))
var _ = (fs.NodeLookuper)((*goProMedium)(nil))
var _ = (fs.NodeReaddirer)((*goProMedium)(nil))
var _ = (fs.NodeMkdirer)((*goProMedium)(nil))
var _ = (fs.NodeCreater)((*goProMedium)(nil))
var _ = (fs.NodeUnlinker)((*goProMedium)(nil))
