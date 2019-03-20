/* Used to handle missed file-coin's pkgs .
 * We already have some pkgs downloaded before VPN closed, and packages we needed maybe just an alias path of already existed packages, like gx/ipfs/22342342342/ => gx/ipfs/33333344334/
 * So we take needed-package-hash & needed-package-name from /tmp/2
 */
package main

import (
	"./format"
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"regexp"
	"runtime"
	"strings"
	"sync"
)

const (
	TMPFILE2        = "/tmp/2"
	TMPFILE4        = "/tmp/4"
	IPFS_ROOT_DIR   = "/root/go/src/gx/ipfs/"
	GITHUB_ROOT_DIR = "/root/go/src/github.com/"
)

// Index: pkg_name => pkg_hash
var existPkgs map[string]string

// Set: pkg_hashes
var existPkgHashes map[string]struct{}

var wg sync.WaitGroup

func init() {
	existPkgs = make(map[string]string)
	existPkgHashes = make(map[string]struct{})
}

func scanExistPkgs() error {
	log.Printf("Scanning '%s' to know what pkgs we already have.\n", IPFS_ROOT_DIR)

	// Read pkgs' hash under $IPFS_ROOT_DIR
	files, err := ioutil.ReadDir(IPFS_ROOT_DIR)
	if err != nil {
		return err
	}

	// Read files under these dirs
	for _, f := range files {
		if matched, _ := regexp.MatchString("[a-zA-Z0-9]{46}", f.Name()); !matched {
			fmt.Printf("----> ignore [%s]\n", f.Name())
			continue
		}
		subDirs, _ := ioutil.ReadDir(IPFS_ROOT_DIR + "/" + f.Name())
		for _, subDir := range subDirs {
			existPkgs[subDir.Name()] = f.Name()
			existPkgHashes[f.Name()] = struct{}{}
		}
	}

	log.Printf("Scanned %d ipfs pkgs, and %d pkgHashes.\n", len(existPkgs), len(existPkgHashes))

	return nil
}

func createSoftLink(existPath, linkPath string) error {
	/*
		cmdStr := fmt.Sprintf("ln -s %s %s", IPFS_ROOT_DIR+existHash, IPFS_ROOT_DIR+linkHash)
		fmt.Println(cmdStr)
		return nil
	*/
	cmd := exec.Command("/bin/bash", "-c", fmt.Sprintf("ln -s %s %s", existPath, linkPath))

	var out bytes.Buffer
	cmd.Stdout = &out

	err := cmd.Run()
	if err != nil {
		return err
	}
	fmt.Println(out.String())
	return nil
}

func handleIOErrRet(err error) {
	if err != io.EOF {
		fmt.Printf("Result Error: %s\n", err)
	}
}

func findInGithubDir(hash, name string) error {

	cmd := exec.Command("/bin/bash", "-c", fmt.Sprintf("find %s -name %s", GITHUB_ROOT_DIR, name))
	var out bytes.Buffer
	cmd.Stdout = &out

	err := cmd.Run()
	if err != nil {
		return err
	}

	res := strings.Split(out.String(), "\n")
	if len(res) < 2 {
		fmt.Printf("Can't find '%s' in %s/github.com", name, runtime.GOROOT())
	} else if len(res) == 2 {
		srcPath := res[0]
		dstPath := IPFS_ROOT_DIR + hash
		fmt.Printf("Find '%s' in %s/github.com. Create dir '%s' and soft link from '%s'\n", name, runtime.GOROOT(), dstPath, srcPath)
		// mkdir
		if err = os.Mkdir(dstPath, os.ModeDir); err != nil {
			return err
		}
		// create linke
		if err = createSoftLink(srcPath, dstPath); err != nil {
			return err
		}
	} else {
		fmt.Printf("More than one '%s' of '%s' found! Need manual handle.\n [%s]\n", name, hash, out.String())
	}

	return nil
}

func handleMissing(hashNameGetter format.HashNameGetter) {

	defer wg.Done()

	checkedHashList := make(map[string]struct{})

	var targetPkgHash, targetPkgName string
	var err error
	for {
		if targetPkgHash, targetPkgName, err = hashNameGetter.GetHashName(); err != nil {
			handleIOErrRet(err)
			return
		}

		// This duplicated 'hash' has been handled
		if _, ok := checkedHashList[targetPkgHash]; ok {
			continue
		}
		// Hash dir needed to be created already exists, pass it
		if _, ok := existPkgHashes[targetPkgHash]; ok {
			continue
		}

		// target pkg not exists? can't do further link
		existPkgHash, ok := existPkgs[targetPkgName]
		if !ok {
			fmt.Printf("NOT found src '%s' in ipfs dir. Check 'github.com' \n", targetPkgName)
			if err := findInGithubDir(targetPkgHash, targetPkgName); err != nil {
				handleIOErrRet(err)
			}
			continue
		} else if existPkgHash == targetPkgHash {
			fmt.Printf("'%s' already exists. Check next.\n")
			continue
		}

		// We have a new pkg hash needed to create soft link
		fmt.Println("OK, found src ", targetPkgName, " under dir-hash ", existPkgHash)
		createSoftLink(IPFS_ROOT_DIR+existPkgHash, IPFS_ROOT_DIR+targetPkgHash)

		checkedHashList[targetPkgHash] = struct{}{}
	}
}

func main() {

	if err := scanExistPkgs(); err != nil {
		log.Fatalln(err)
	}

	/*
		fmt.Printf("Handling [%s]...\n", TMPFILE2)
		getter := format.NewFormat1Getter(TMPFILE2)
		wg.Add(1)
		go handleMissing(getter)
	*/

	fmt.Printf("\nHandling [%s]...\n", TMPFILE4)
	getter2 := format.NewFormat2Getter(TMPFILE4)
	wg.Add(1)
	go handleMissing(getter2)

	wg.Wait()
}
