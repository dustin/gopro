package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"sync"
	"time"

	"syscall"

	"github.com/dustin/httputil"
	"github.com/hanwen/go-fuse/v2/fs"
	"github.com/hanwen/go-fuse/v2/fuse"
)

const (
	SIGINFO    = syscall.Signal(29)
	maxMissing = 10
)

type GoProRoot struct {
	sources       stringListFlag
	cacheDir      string
	proxyDir      string
	baseURL       string
	tree          fstree
	current       map[string]map[string]int
	recentMissing []string
	fs.Inode
	mu sync.Mutex
}

func (fs *GoProRoot) trackOpen(f string, p string) func() {
	log.Printf("%v opened %v", p, f)
	fs.mu.Lock()
	defer fs.mu.Unlock()

	if fs.current == nil {
		fs.current = map[string]map[string]int{}
	}
	if fs.current[f] == nil {
		fs.current[f] = map[string]int{}
	}
	fs.current[f][p] = fs.current[f][p] + 1

	return func() {
		fs.trackClose(f, p)
	}
}

func (fs *GoProRoot) trackClose(f string, p string) {
	log.Printf("%v closed %v", p, f)
	fs.mu.Lock()
	defer fs.mu.Unlock()

	fs.current[f][p] = fs.current[f][p] - 1
	if fs.current[f][p] == 0 {
		delete(fs.current[f], p)
	}
	if len(fs.current[f]) == 0 {
		delete(fs.current, f)
	}
}

func (fs *GoProRoot) wasMissing(name string) {
	fs.mu.Lock()
	defer fs.mu.Unlock()
	if len(fs.recentMissing) > 0 && fs.recentMissing[len(fs.recentMissing)-1] == name {
		return
	}
	fs.recentMissing = append(fs.recentMissing, name)
	if len(fs.recentMissing) > maxMissing {
		fs.recentMissing = fs.recentMissing[:maxMissing]
	}
}

func (gr *GoProRoot) Lookup(ctx context.Context, name string, out *fuse.EntryOut) (*fs.Inode, syscall.Errno) {
	if ch := gr.GetChild(name); ch != nil {
		return ch, 0
	}

	if name == ".refresh" {
		return gr.NewPersistentInode(ctx, &refreshFile{}, fs.StableAttr{}), 0
	}

	return nil, syscall.ENOENT
}

func (gp *GoProRoot) refresh(ctx context.Context) (err error) {
	log.Printf("Refreshing")
	defer func() {
		if err == nil {
			log.Printf("Refreshed")
		} else {
			log.Printf("Error refreshing: %v", err)
		}
	}()
	res, err := fetchList(ctx, gp.baseURL)
	if err != nil {
		return fmt.Errorf("failed to get media list: %v", err)
	}
	gp.mu.Lock()
	defer gp.mu.Unlock()
	gp.tree = treeMedia(res)
	gp.updateTree(ctx)
	return nil
}

func (gr *GoProRoot) updateTree(ctx context.Context) {
	for y, ms := range gr.tree {
		p := &gr.Inode

		yn := fmt.Sprintf("%v", y)
		yearInode := p.GetChild(yn)
		if yearInode == nil {
			yearInode = p.NewPersistentInode(ctx, &fs.Inode{}, fs.StableAttr{Mode: fuse.S_IFDIR})
			p.AddChild(yn, yearInode, true)
		}

		for m, ids := range ms {
			m := m
			mn := fmt.Sprintf("%02d", m)
			monthInode := yearInode.GetChild(mn)
			if monthInode == nil {
				monthInode = yearInode.NewPersistentInode(ctx, &fs.Inode{}, fs.StableAttr{Mode: fuse.S_IFDIR})
				yearInode.AddChild(mn, monthInode, true)
			}

			for id, medium := range ids {
				if monthInode.GetChild(id) != nil {
					continue
				}

				id := id
				medium := medium
				medContainer := &goProMedium{captured: medium.Captured, id: medium.Id}
				mediumInode := monthInode.NewPersistentInode(ctx, medContainer, fs.StableAttr{Mode: fuse.S_IFDIR})
				monthInode.AddChild(id, mediumInode, true)

				for _, f := range medium.Files {
					f := f
					gpf := &goProFile{gpf: f, parent: medContainer}
					ch := mediumInode.NewPersistentInode(ctx, gpf, fs.StableAttr{})
					mediumInode.AddChild(f.Name, ch, true)
				}
			}
		}
	}
}

func (gr *GoProRoot) bgRefresh(ctx context.Context) {
	for range time.After(time.Hour) {
		gr.refresh(ctx)
	}
}

func (fs *GoProRoot) Statfs(ctx context.Context, out *fuse.StatfsOut) syscall.Errno {
	var totalSize, numFiles uint64
	for _, ms := range fs.tree {
		for _, ids := range ms {
			for _, medium := range ids {
				for _, f := range medium.Files {
					totalSize += f.Size
					numFiles += 1
				}
			}
		}
	}
	const blockSize = 4096
	out.Blocks = totalSize / blockSize
	out.Files = numFiles
	out.Bsize = blockSize
	return 0
}

func (fs *GoProRoot) info(ch chan os.Signal, tracker *httputil.HTTPTracker) {
	for range ch {
		func() {
			tracker.Report(os.Stdout)
			fs.mu.Lock()
			defer fs.mu.Unlock()

			if len(fs.current) > 0 {
				fmt.Printf("Open files:\n")
				for f, procs := range fs.current {
					fmt.Printf("  * %v:\n", f)
					for proc, n := range procs {
						fmt.Printf("    * %v: %v\n", proc, n)
					}
				}
			}

			if len(fs.recentMissing) > 0 {
				fmt.Printf("Recent missing:\n")
				for _, n := range fs.recentMissing {
					fmt.Printf(" * %v\n", n)
				}
			}
		}()
	}
}

func (r *GoProRoot) OnAdd(ctx context.Context) {
	if err := r.refresh(ctx); err != nil {
		log.Printf("Error initializing root: %v", err)
	}
}

func (r *GoProRoot) Getattr(ctx context.Context, fh fs.FileHandle, out *fuse.AttrOut) syscall.Errno {
	out.Mode = 0755
	return 0
}

var _ = (fs.NodeGetattrer)((*GoProRoot)(nil))
var _ = (fs.NodeOnAdder)((*GoProRoot)(nil))
var _ = (fs.NodeLookuper)((*GoProRoot)(nil))
var _ = (fs.NodeGetattrer)((*GoProRoot)(nil))
var _ = (fs.NodeStatfser)((*GoProRoot)(nil))
