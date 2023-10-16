package main

import (
	"testing"
	"encoding/json"
	"os"
)

func readExample(t *testing.T) []Medium {
	rv := []Medium{}
	f, err := os.Open("files.json")
	if err != nil {
		t.Fatalf("Error opening example file:  %v", err)
	}
	defer f.Close()
	if err = json.NewDecoder(f).Decode(&rv); err != nil {
		t.Fatalf("Error decoding example file:  %v", err)
	}
	return rv
}

func TestTree(t *testing.T) {
	ex := readExample(t)
	var totalMedia, totalFiles int
	for _, m := range ex {
		totalMedia += 1
		totalFiles += len(m.Files)
	}

	var gotMedia, gotFiles int
	for _, yd := range treeMedia(ex) {
		for _, md := range yd {
			gotMedia += len(md)
			for _, ms := range md {
				gotFiles += len(ms.Files)
			}
		}
	}

	if totalMedia != gotMedia {
		t.Errorf("tree media = %v, want %v", gotMedia, totalMedia)
	}
	if totalFiles != gotFiles {
		t.Errorf("got files = %v, want %v", gotFiles, totalFiles)
	}
}
