package main

import (
	"encoding/json"
	"net/http"
	"time"
)

const (
	mediaListURL = "/api/files"
	retrieveURL  = "/api/files/"
)

type File struct {
	Size    uint64 `json:"file_size"`
	Name    string `json:"filename"`
	Section string `json:"section"`
	Label   string `json:"label"`
	Num     int    `json:"item_number"`
	Type    string `json:"type"`
}

type Medium struct {
	Id       string
	Captured time.Time `json:"captured_at"`
	Created  time.Time `json:"created_at"`
	Size     uint64    `json:"file_size"`
	Files    []File    `json:"files"`
}

func fetchList(baseu string) ([]Medium, error) {
	res, err := http.Get(baseu + mediaListURL)
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()

	msg := []Medium{}
	if err = json.NewDecoder(res.Body).Decode(&msg); err != nil {
		return nil, err
	}
	return msg, nil
}

func fetchURLs(baseu string, mid string) (map[File]string, error) {
	res, err := http.Get(baseu + retrieveURL + mid)
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()

	files := []struct {
		Filename string `json:"filename"`
		URL      string `json:"url"`
		FileData File   `json:"fileData"`
	}{}
	if err = json.NewDecoder(res.Body).Decode(&files); err != nil {
		return nil, err
	}
	rv := map[File]string{}
	for _, f := range files {
		rv[f.FileData] = f.URL
	}
	return rv, nil
}

type fstree map[int]map[time.Month]map[string]Medium

func treeMedia(media []Medium) fstree {
	rv := fstree{}
	for _, m := range media {
		m := m
		year := rv[m.Captured.Year()]
		if year == nil {
			year = map[time.Month]map[string]Medium{}
			rv[m.Captured.Year()] = year
		}
		month := year[m.Captured.Month()]
		if month == nil {
			month = map[string]Medium{}
		}
		month[m.Id] = m
		year[m.Captured.Month()] = month
	}
	return rv
}
