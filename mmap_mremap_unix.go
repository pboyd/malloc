//go:build darwin

package malloc

import (
	"errors"
	"unsafe"

	"golang.org/x/sys/unix"
)

func mremap(oldData []byte, newLength int, prot int, flags int) (newData []byte, ext []byte, err error) {
	startAddress := unsafe.Pointer(unsafe.SliceData(oldData))
	extStart := unsafe.Pointer(uintptr(startAddress) + uintptr(len(oldData)))
	extLength := newLength - len(oldData)

	extPtr, err := unix.MmapPtr(-1, 0, extStart, uintptr(extLength), prot, flags|unix.MAP_FIXED)
	if err != nil {
		if errors.Is(err, unix.ENOMEM) {
			return nil, nil, nil
		}
		return nil, nil, err
	}

	ext = unsafe.Slice((*byte)(extPtr), extLength)
	newData = unsafe.Slice((*byte)(startAddress), newLength)
	return
}
