package main

import (
	"testing"
)

func TestBlockCount(t *testing.T) {
	const sixtyfour = 64 * BlockSize

	tests := []struct {
		name     string
		in, want uint64
	}{
		{"64", sixtyfour, 64},
		{"64-1", sixtyfour - 1, 64},
		{"64+1", sixtyfour + 1, 65},
	}

	for _, test := range tests {
		if got := blockCount(test.in); got != 64 {
			t.Errorf("blockCount(%v) = %v, want %v", test.name, got, test.want)
		}
	}
}

func TestBlockOffset(t *testing.T) {
	const sixtyfour = 64 * BlockSize

	tests := []struct {
		name     string
		in, want uint64
	}{
		{"64", sixtyfour, 0},
		{"64-1", sixtyfour - 1, BlockSize - 1},
		{"64+1", sixtyfour + 1, 1},
	}

	for _, test := range tests {
		if got := blockCount(test.in); got != 64 {
			t.Errorf("blockOffset(%v) = %v, want %v", test.name, got, test.want)
		}
	}
}
