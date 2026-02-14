package malloc

import (
	"errors"

	"golang.org/x/sys/unix"
)

func mremap(oldData []byte, newLength int, prot int, flags int) (newData []byte, ext []byte, err error) {
	buf, err := unix.Mremap(oldData, newLength, 0)
	if err != nil {
		if errors.Is(err, unix.ENOMEM) {
			return nil, nil, nil
		}
		return nil, nil, err
	}
	return buf, nil, nil
}
