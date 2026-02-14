//go:build !linux && !darwin

package malloc

func mremap(oldData []byte, newLength int) ([]byte, error) {
	// Just return nil and we'll fallback like mremap failed
	return nil, nil
}
