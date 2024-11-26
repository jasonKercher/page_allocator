#+private

package page_allocator

import "base:runtime"
import "base:intrinsics"

import "core:mem"
import "core:mem/virtual"

_GRANULARITY_MIN :: 64 * mem.Kilobyte
_GRANULARITY_MAX :: 64 * mem.Kilobyte

@(require_results)
_page_aligned_alloc :: proc(size, alignment, _: int,
                            flags: Page_Allocator_Flags) -> (memory: []byte, err: mem.Allocator_Error) {
	assert(alignment <= GRANULARITY_MAX)
	assert(runtime.is_power_of_two(alignment))
	if size == 0 {
		return nil, .Invalid_Argument
	}
	size_full := mem.align_forward_int(size, GRANULARITY_MIN)
	memory = virtual.reserve_and_commit(uint(size_full)) or_return
	return memory[:size], nil
}


@(require_results)
_page_aligned_resize :: proc(old_ptr: rawptr,
                             old_size, new_size, new_alignment, _: int,
                             flags: Page_Allocator_Flags) -> (memory: []byte, err: mem.Allocator_Error) {
	assert(new_alignment <= GRANULARITY_MAX)
	assert(runtime.is_power_of_two(new_alignment))

	if old_ptr == nil || !mem.is_aligned(old_ptr, GRANULARITY_MIN) {
		return mem.byte_slice(old_ptr, old_size), .Invalid_Pointer
	}
	if new_size == 0 {
		return nil, page_free(old_ptr, old_size)
	}

	old_size_full := mem.align_forward_int(old_size, GRANULARITY_MIN)
	new_size_full  := mem.align_forward_int(new_size, GRANULARITY_MIN)

	// Can we meet the request with existing memory?
	if new_size <= old_size_full {
		memory = mem.byte_slice(old_ptr, new_size)
		if new_size > old_size {
			if .Uninitialized_Memory not_in flags {
				mem.zero_slice(memory[old_size:])
			}
		} else if new_size_full < old_size_full {
			unused := uintptr(old_ptr) + uintptr(new_size_full)
			virtual.decommit(rawptr(unused), uint(old_size_full - new_size_full))
		}
		return
	}

	// Can we resize in place (previously decommitted)?
	commit_size := new_size_full - old_size_full + GRANULARITY_MIN
	commit_addr := uintptr(old_ptr) + uintptr(commit_size) - GRANULARITY_MIN
	if virtual.commit(rawptr(commit_addr), uint(commit_size)) == nil {
		return mem.byte_slice(old_ptr, new_size), nil
	}

	// allocate -> copy -> free
	memory, err = page_aligned_alloc(new_size, new_alignment, flags=flags)
	if err != nil {
		return mem.byte_slice(old_ptr, old_size), err
	}

	mem.copy_non_overlapping(&memory[0], old_ptr, old_size)
	if .Never_Free not_in flags {
		virtual.release(old_ptr, uint(old_size_full))
	}
	return
}

