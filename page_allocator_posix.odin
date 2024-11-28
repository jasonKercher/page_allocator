#+build darwin, netbsd, freebsd, openbsd
#+private

package page_allocator

import "base:runtime"
import "base:intrinsics"

import "core:mem"
import "core:mem/virtual"

import "core:sys/posix"

_GRANULARITY_MIN :: 4 * mem.Kilobyte
_GRANULARITY_MAX :: 1 * mem.Gigabyte

@(require_results)
_page_aligned_alloc :: proc(size, alignment: int,
                            granularity: int,
                            flags: Page_Allocator_Flags) -> (memory: []byte, err: mem.Allocator_Error) {
	assert(granularity >= GRANULARITY_MIN)
	assert(runtime.is_power_of_two(granularity))
	assert(runtime.is_power_of_two(alignment))
	if size == 0 {
		return nil, .Invalid_Argument
	}

	// TODO: try native huge pages?

	size_full := mem.align_forward_int(size, granularity)

	alignment := alignment
	alignment = max(alignment, GRANULARITY_MIN)
	if alignment == GRANULARITY_MIN {
		return virtual.reserve_and_commit(uint(size_full))
	}

	// We gave ourselves enough extra space to retrieve the size AND seek
	// to the desired alignment.
	mapping_size := size_full + (alignment - GRANULARITY_MIN)
	reserve := virtual.reserve(uint(mapping_size)) or_return

	base := uintptr(&reserve[0])
	addr := mem.align_forward_int(int(base), alignment)

	base_waste := addr - int(base)
	memory = reserve[base_waste:base_waste + size]

	if base_waste > 0 {
		virtual.release(&reserve[0], uint(base_waste))
	}

	post_waste := (alignment - GRANULARITY_MIN) - base_waste
	if post_waste > 0 {
		post_waste_ptr := rawptr(uintptr(addr + size_full))
		virtual.release(post_waste_ptr, uint(post_waste))
	}

	err = virtual.commit(&memory[0], uint(size_full))
	if err != nil {
		virtual.release(&memory[0], uint(size_full))
		memory = nil
	}

	return
}


@(require_results)
_page_aligned_resize :: proc(old_ptr: rawptr,
                             old_size, new_size, new_alignment, granularity: int,
                             flags: Page_Allocator_Flags) -> (memory: []byte, err: mem.Allocator_Error) {
	assert(granularity >= GRANULARITY_MIN)
	assert(runtime.is_power_of_two(granularity))
	assert(runtime.is_power_of_two(new_alignment))

	if old_ptr == nil || !mem.is_aligned(old_ptr, granularity) {
		return mem.byte_slice(old_ptr, old_size), .Invalid_Pointer
	}
	if new_size == 0 {
		return nil, page_free(old_ptr, old_size)
	}

	old_size_full := mem.align_forward_int(old_size, granularity)
	new_size_full  := mem.align_forward_int(new_size, granularity)

	new_alignment := new_alignment
	new_alignment = max(new_alignment, GRANULARITY_MIN)

	// Can we meet the request with existing memory?
	if new_size <= old_size_full {
		memory = mem.byte_slice(old_ptr, new_size)
		if new_size > old_size {
			if .Uninitialized_Memory not_in flags {
				mem.zero_slice(memory[old_size:])
			}
		} else if new_size_full < old_size_full {
			unused := uintptr(old_ptr) + uintptr(new_size_full)
			virtual.release(rawptr(unused), uint(old_size_full - new_size_full))
		}
		return
	}

	// TODO: Native resize in place?

	memory, err = page_aligned_alloc(new_size, new_alignment, flags, granularity)
	if err != nil {
		return mem.byte_slice(old_ptr, old_size), err
	}

	mem.copy_non_overlapping(&memory[0], old_ptr, old_size)
	if .Never_Free not_in flags {
		virtual.release(old_ptr, uint(old_size_full))
	}

	// TODO: Native huge page collapse?

	return
}
