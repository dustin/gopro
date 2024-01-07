package main

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"time"
)

const (
	BlockSize = 8 * 1024 * 1024
)

func blockCount(s uint64) uint64 {
	return (s + 1) / BlockSize
}

func blockOffset(s uint64) uint64 {
	return s % BlockSize
}

func blockLoc(s uint64) (uint64, uint64) {
	return blockCount(s), blockOffset(s)
}

func blockRange(s uint64) (uint64, uint64) {
	return s * BlockSize, (s + 1) * BlockSize
}

func fillHole(ctx context.Context, w io.WriteSeeker, u string, from uint64, to uint64) (int64, error) {
	ctx, cancel := context.WithTimeout(ctx, 15*time.Second)
	defer cancel()

	if _, err := w.Seek(int64(from), io.SeekStart); err != nil {
		return 0, err
	}
	req, err := http.NewRequestWithContext(ctx, "GET", u, nil)
	if err != nil {
		return 0, err
	}
	rng := fmt.Sprintf("bytes=%v-%v", from, to)
	// log.Printf("Fetching with Range: %v", rng)
	req.Header.Set("Range", rng)
	res, err := http.DefaultClient.Do(req)
	if err != nil {
		return 0, err
	}
	defer res.Body.Close()
	if res.StatusCode != 206 && res.StatusCode != 200 {
		return 0, err
	}
	return io.Copy(w, res.Body)
}
