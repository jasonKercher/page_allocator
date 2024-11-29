#+private

package page_allocator

import "base:runtime"
import "base:intrinsics"

import "core:mem"
import "core:mem/virtual"

import "core:sys/linux"

_GRANULARITY_MIN :: 4 * mem.Kilobyte
_GRANULARITY_MAX :: 1 * mem.Gigabyte

// log2 of the huge page size << 26
MAP_HUGE_2MB : i32 : 21 << 26
MAP_HUGE_1GB : i32 : 30 << 26

@(require_results)
_page_aligned_alloc :: proc(size, alignment, granularity: int,
                            flags: Page_Allocator_Flags) -> (memory: []byte, err: mem.Allocator_Error) {
	seek_alignment_and_commit :: proc(reserve: []byte, size, alignment, granularity: int) -> (memory: []byte, err: mem.Allocator_Error) {
		size_full := mem.align_forward_int(size, granularity)
		base := uintptr(&reserve[0])
		addr := mem.align_forward_int(int(base), alignment)

		base_waste := addr - int(base)
		memory = reserve[base_waste:base_waste + size]

		if base_waste > 0 {
			virtual.release(&reserve[0], uint(base_waste))
		}

		post_waste := (alignment - granularity) - base_waste
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

	assert(granularity >= GRANULARITY_MIN)
	assert(granularity <= GRANULARITY_MAX)
	assert(runtime.is_power_of_two(granularity))
	assert(alignment <= GRANULARITY_MAX)
	assert(runtime.is_power_of_two(alignment))
	if size == 0 {
		return nil, .Invalid_Argument
	}

	alignment := alignment
	alignment = max(alignment, GRANULARITY_MIN)

	size_full := mem.align_forward_int(size, granularity)
	if alignment == GRANULARITY_MIN {
		memory, err = virtual.reserve_and_commit(uint(size_full))
		if err != nil {
			memory = memory[:size]
		}
		return
	}

	if .Allow_Large_Pages in flags {
		if granularity >= mem.Gigabyte && size >= mem.Gigabyte {
			raw_map_flags := i32(MAP_HUGE_1GB)
			map_flags := transmute(linux.Map_Flags)(raw_map_flags)
			map_flags += {.ANONYMOUS, .PRIVATE, .HUGETLB}

			ptr, errno := linux.mmap(0, uint(size_full), {.READ, .WRITE}, map_flags)
			if ptr != nil && errno == nil {
				return mem.byte_slice(ptr, size), nil
			}
		} else if granularity > 2 * mem.Megabyte && size > 2 * mem.Megabyte {
			raw_map_flags := i32(MAP_HUGE_2MB)
			map_flags := transmute(linux.Map_Flags)(raw_map_flags)
			map_flags += {.ANONYMOUS, .PRIVATE, .HUGETLB}

			if alignment < 2 * mem.Megabyte {
				ptr, errno := linux.mmap(0, uint(size_full), {.READ, .WRITE}, map_flags)
				if ptr != nil && errno == nil {
					return mem.byte_slice(ptr, size), nil
				}
			} else {
				reserve_size := size_full + (alignment - 2 * mem.Megabyte)
				reserve_ptr, errno := linux.mmap(0, uint(size_full), {}, map_flags)
				reserve := mem.byte_slice(reserve_ptr, reserve_size)
				if reserve_ptr != nil && errno == nil {
					return seek_alignment_and_commit(reserve, size, alignment, granularity)
				}
			}
		}
	}

	// We gave ourselves enough extra space to retrieve the size AND seek
	// to the desired alignment.
	reserve_size := size_full + (alignment - GRANULARITY_MIN)
	reserve := virtual.reserve(uint(reserve_size)) or_return
	return seek_alignment_and_commit(reserve, size, alignment, granularity)
}


@(require_results)
_page_aligned_resize :: proc(old_ptr: rawptr,
                             old_size, new_size, new_alignment, granularity: int,
                             flags: Page_Allocator_Flags) -> (memory: []byte, err: mem.Allocator_Error) {
	assert(granularity >= GRANULARITY_MIN)
	assert(granularity <= GRANULARITY_MAX)
	assert(runtime.is_power_of_two(granularity))
	assert(new_alignment <= GRANULARITY_MAX)
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

	// Can we resize in place?
	if mem.is_aligned(old_ptr, new_alignment) {
		memory, err = _resize_allocation(old_ptr, uint(old_size_full), uint(new_size_full), may_move=false)
		if err == nil {
			return memory[:new_size], nil
		}
	}

	// Do we *need* to do a manual allocate -> copy -> free?
	if .Never_Free in flags || new_alignment > GRANULARITY_MIN {
		memory, err = page_aligned_alloc(new_size, new_alignment, granularity, flags)
		if err != nil {
			return mem.byte_slice(old_ptr, old_size), err
		}

		mem.copy_non_overlapping(&memory[0], old_ptr, old_size)
		if .Never_Free not_in flags {
			virtual.release(old_ptr, uint(old_size_full))
		}
		return
	}

	memory, err = _resize_allocation(old_ptr, uint(old_size_full), uint(new_size_full), may_move=true)
	if err != nil {
		return mem.byte_slice(old_ptr, old_size), err
	}
	memory = memory[:new_size]

	if .Allow_Large_Pages in flags &&
	   new_size > 2 * mem.Megabyte &&
	   new_size - old_size > new_size / 2 {
		attempt_huge_page_collapse(&memory[0], len(memory))
	}

	return
}

_resize_allocation :: proc (old_data: rawptr, old_size, new_size: uint, may_move: bool) -> (data: []byte, err: mem.Allocator_Error) {
	flags: linux.MRemap_Flags
	if may_move {
		flags += {.MAYMOVE}
	}

	addr, errno := linux.mremap(old_data, old_size, new_size, flags)
	#partial switch errno {
	case .EINVAL, .EFAULT:
		return (cast([^]byte)old_data)[:old_size], .Invalid_Pointer
	case .ENOMEM, .EAGAIN:
		return (cast([^]byte)old_data)[:old_size], .Out_Of_Memory
	}
	return (cast([^]byte)addr)[:new_size], nil
}

attempt_huge_page_collapse :: proc(addr: rawptr, size: int) {
	if size < 2 * mem.Megabyte {
		return
	}
	
	size_full := mem.align_forward_int(size, 2 * mem.Megabyte)
	huge_page_addr := mem.align_forward(addr, 2 * mem.Megabyte)
	huge_page_size := uintptr(size_full) - (uintptr(huge_page_addr) - uintptr(addr))
	if huge_page_size < 2 * mem.Megabyte {
		return
	}

	// This is purely an optimization that attempts to use Transparent Huge Pages (THPs).
	// THPs have the same semantics as regular 4K pages, so we don't need to track them.
	_ = linux.madvise(huge_page_addr, uint(huge_page_size), .COLLAPSE)
}
