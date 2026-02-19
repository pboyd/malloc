//go:build linux || netbsd

package malloc

import (
	"errors"
	"unsafe"

	"golang.org/x/sys/unix"
)

func mremap(oldData []byte, newLength int) ([]byte, error) {
	ptr, err := unix.MremapPtr(unsafe.Pointer(unsafe.SliceData(oldData)), uintptr(len(oldData)), nil, uintptr(newLength), 0)
	if err != nil {
		if errors.Is(err, unix.ENOMEM) {
			return nil, nil
		}
		return nil, err
	}
	return unsafe.Slice((*byte)(unsafe.Pointer(ptr)), newLength), nil
}
