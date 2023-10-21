package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	"os/signal"

	"syscall"

	"github.com/dustin/httputil"
	"github.com/hanwen/go-fuse/v2/fs"
	"github.com/hanwen/go-fuse/v2/fuse"
	ps "github.com/mitchellh/go-ps"
)

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

	opts := &fs.Options{
		MountOptions: fuse.MountOptions{
			Debug:         *debug,
			FsName:        "GoProCloud",
			Name:          "GoPro",
			DisableXAttrs: true,
		},
	}

	go gproot.bgRefresh(context.Background())

	fsroot := flag.Arg(0)
	log.Printf("Mounting at %v", fsroot)
	server, err := fs.Mount(fsroot, gproot, opts)
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
