package main

import (
	"context"
	"fmt"
	"log"

	"syscall"

	"github.com/hanwen/go-fuse/v2/fs"
	"github.com/hanwen/go-fuse/v2/fuse"
)

type refreshFile struct {
	fs.Inode
	msg string
}

var _ = (fs.NodeOpener)((*refreshFile)(nil))
var _ = (fs.NodeReader)((*refreshFile)(nil))

func (r *refreshFile) Open(ctx context.Context, flags uint32) (fh fs.FileHandle, flg uint32, errno syscall.Errno) {
	r.msg = "ok"
	root := r.Root()
	gr, ok := root.Root().Operations().(*GoProRoot)
	if ok {
		if err := gr.refresh(ctx); err != nil {
			r.msg = fmt.Sprintf("Failed to refresh: %v", err)
		}
	} else {
		r.msg = fmt.Sprintf("Root isn't GoProRoot, it's %v", r)
	}
	return r, fuse.FOPEN_KEEP_CACHE, 0
}

func (r *refreshFile) Read(ctx context.Context, fh fs.FileHandle, dest []byte, off int64) (fuse.ReadResult, syscall.Errno) {
	log.Printf("Reading should return %v", r.msg)
	return fuse.ReadResultData([]byte(r.msg)), 0
}
