package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"os/signal"
	"path/filepath"
	"sync"
	"time"

	"syscall"

	"github.com/dustin/httputil"
	"github.com/hanwen/go-fuse/v2/fs"
	"github.com/hanwen/go-fuse/v2/fuse"
	ps "github.com/mitchellh/go-ps"
)

const SIGINFO = syscall.Signal(29)

type GoProRoot struct {
	sources  stringListFlag
	cacheDir string
	proxyDir string
	baseURL  string
	tree     fstree
	current  map[File]map[string]int
	fs.Inode
	mu sync.Mutex
}

func (fs *GoProRoot) trackOpen(f File, p string) {
	fs.mu.Lock()
	defer fs.mu.Unlock()

	if fs.current == nil {
		fs.current = map[File]map[string]int{}
	}
	if fs.current[f] == nil {
		fs.current[f] = map[string]int{}
	}
	fs.current[f][p] = fs.current[f][p] + 1
}

func (fs *GoProRoot) trackClose(f File, p string) {
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

			for f, procs := range fs.current {
				fmt.Printf("Procs with %v open:\n", f.Name)
				for proc, n := range procs {
					fmt.Printf("\t%v: %v\n", proc, n)
				}
			}
		}()
	}
}

type goProMedium struct {
	fs.Inode
	id       string
	captured time.Time

	origUrls map[File]string
	mu       sync.Mutex
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

func (gm *goProMedium) Getattr(ctx context.Context, f fs.FileHandle, out *fuse.AttrOut) syscall.Errno {
	out.Mode = 0555
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

func procName(ctx context.Context) string {
	if caller, ok := fuse.FromContext(ctx); ok {
		proc, err := ps.FindProcess(int(caller.Pid))
		if err != nil {
			log.Printf("Error finding caller name: %v", err)
			return ""
		}
		if proc == nil {
			return "<nilproc>"
		}
		return proc.Executable()
	}
	return ""
}

func ignore(ctx context.Context) bool {
	return procName(ctx) == "QuickLookSatelli"
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

var _ = (fs.NodeOpener)((*goProFile)(nil))
var _ = (fs.NodeGetattrer)((*goProFile)(nil))
var _ = (fs.NodeReader)((*goProFile)(nil))
var _ = (fs.NodeReleaser)((*goProFile)(nil))

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

func (r *GoProRoot) OnAdd(ctx context.Context) {
	proxyDirs := map[string]bool{}
	for y, ms := range r.tree {
		p := &r.Inode

		yearInode := p.NewPersistentInode(ctx, &fs.Inode{}, fs.StableAttr{Mode: fuse.S_IFDIR})
		p.AddChild(fmt.Sprintf("%v", y), yearInode, true)

		for m, ids := range ms {
			m := m
			monthInode := yearInode.NewPersistentInode(ctx, &fs.Inode{}, fs.StableAttr{Mode: fuse.S_IFDIR})
			yearInode.AddChild(fmt.Sprintf("%02d", m), monthInode, true)
			for id, medium := range ids {
				id := id
				medium := medium
				medContainer := &goProMedium{captured: medium.Captured, id: medium.Id}
				mediumInode := monthInode.NewPersistentInode(ctx, medContainer, fs.StableAttr{Mode: fuse.S_IFDIR})
				monthInode.AddChild(id, mediumInode, true)

				hasVideo := false
				for _, f := range medium.Files {
					f := f
					gpf := &goProFile{gpf: f, parent: medContainer}
					ch := mediumInode.NewPersistentInode(ctx, gpf, fs.StableAttr{})
					mediumInode.AddChild(f.Name, ch, true)
					if f.Type == "mp4" {
						hasVideo = true
					}
				}

				// Proxy handling
				if r.proxyDir != "" && hasVideo {
					proot := filepath.Join(r.proxyDir, fmt.Sprintf("%v/%02d/%v/Proxy", medium.Captured.Year(), medium.Captured.Month(), medium.Id))
					proxyDirs[proot] = true
					loop, err := fs.NewLoopbackRoot(r.proxyDir)
					if err == nil {
						ch := mediumInode.NewPersistentInode(ctx, loop, fs.StableAttr{Mode: fuse.S_IFDIR})
						mediumInode.AddChild("Proxy", ch, true)
					}
				}
			}
		}
	}
	// This can block for a silly amount of time.  We'll do it async.
	go func() {
		log.Printf("Creating %v proxy dirs", len(proxyDirs))
		defer log.Printf("Finished creating proxy dirs")
		for d := range proxyDirs {
			if err := os.MkdirAll(d, 0777); err != nil {
				log.Printf("Failed to create proxy dir %v: %v", d, err)
			}
		}
	}()
}

func (r *GoProRoot) Getattr(ctx context.Context, fh fs.FileHandle, out *fuse.AttrOut) syscall.Errno {
	out.Mode = 0755
	return 0
}

var _ = (fs.NodeGetattrer)((*GoProRoot)(nil))
var _ = (fs.NodeOnAdder)((*GoProRoot)(nil))

type stringListFlag []string

func (i *stringListFlag) String() string {
	return fmt.Sprintf("%v", *i)
}

func (i *stringListFlag) Set(value string) error {
	*i = append(*i, value)
	return nil
}

func main() {
	debug := flag.Bool("debug", false, "print debug data")
	gproot := &GoProRoot{}

	flag.Var(&gproot.sources, "source", "Directory that may contain media (may be repeated)")
	flag.StringVar(&gproot.cacheDir, "cache", "/tmp/gpcache", "Cache directory")
	flag.StringVar(&gproot.proxyDir, "proxydir", "/tmp/gpproxy", "Proxy directory")
	flag.StringVar(&gproot.baseURL, "url", "http://localhost:8008/", "URL to gopro service")
	flag.Parse()
	if len(flag.Args()) < 1 {
		log.Fatal("Usage:\n  goprofs MOUNTPOINT")
	}

	res, err := fetchList(gproot.baseURL)
	if err != nil {
		log.Fatalf("failed to fetch media: %v", err)
	}
	gproot.tree = treeMedia(res)

	opts := &fs.Options{
		MountOptions: fuse.MountOptions{
			Debug:         *debug,
			FsName:        "GoProCloud",
			Name:          "GoPro",
			DisableXAttrs: true,
		},
	}

	server, err := fs.Mount(flag.Arg(0), gproot, opts)
	if err != nil {
		log.Fatalf("Mount fail: %v\n", err)
	}
	ich := make(chan os.Signal)
	signal.Notify(ich, SIGINFO)
	go gproot.info(ich, httputil.InitHTTPTrackerOnly(false))

	tch := make(chan os.Signal)
	signal.Notify(tch, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-tch
		server.Unmount()
	}()
	server.Wait()
}
