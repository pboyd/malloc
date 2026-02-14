// This has been tested on Linux. NetBSD also has mremap, but I haven't tested it. YMMV
//go:build linux || netbsd

package malloc

import (
	"errors"

	"golang.org/x/sys/unix"
)

func mremap(oldData []byte, newLength int) ([]byte, error) {
	buf, err := unix.Mremap(oldData, newLength, 0)
	if err != nil {
		if errors.Is(err, unix.ENOMEM) {
			return nil, nil
		}
		return nil, err
	}
	return buf, nil
}
