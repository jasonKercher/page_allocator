package page_allocator

import "base:runtime"
import "base:intrinsics"

import "core:mem"
import "core:mem/virtual"

// The idea behind the Page_Allocator is to have a low-level lookup-less allocator
// over the operating system functions. We can use the provided size to infer the
// actual page aligned size. We can also *pretend* to have a different page size by
// setting the granularity. This provides greater control over alignment while still
// being able to infer the correct size of the allocation.
//
// NOTE: Because the size is being inferred, allocations that come from new, must
//       provide size with something like mem.free_with_size to release the memory.

Page_Allocator_Flag :: enum {
	Never_Free,
	Allow_Large_Pages,
	Uninitialized_Memory,
	No_Commit, // TODO
}
Page_Allocator_Flags :: bit_set[Page_Allocator_Flag; u8]

Page_Allocator :: struct {
	flags: Page_Allocator_Flags,
	granularity_log2: u8,
}

GRANULARITY_MIN :: _GRANULARITY_MIN
GRANULARITY_MAX :: _GRANULARITY_MAX

@(require_results)
page_aligned_alloc :: proc(size: int,
                           alignment: int = GRANULARITY_MIN,
                           granularity: int = GRANULARITY_MIN,
                           flags: Page_Allocator_Flags = {}) -> (memory: []byte, err: mem.Allocator_Error) {
	return _page_aligned_alloc(size, alignment, granularity, flags)
}

@(require_results)
page_aligned_resize :: proc(old_ptr: rawptr,
                            old_size, new_size: int,
                            new_alignment: int = GRANULARITY_MIN,
                            granularity: int = GRANULARITY_MIN,
                            flags: Page_Allocator_Flags = {}) -> (memory: []byte, err: mem.Allocator_Error) {
	return _page_aligned_resize(old_ptr, old_size, new_size, new_alignment, granularity, flags)
}

page_free :: proc(p: rawptr, size: int, granularity := GRANULARITY_MIN, flags: Page_Allocator_Flags = {}) -> mem.Allocator_Error {
	assert(granularity >= GRANULARITY_MIN)
	assert(runtime.is_power_of_two(granularity))
	if p == nil || !mem.is_aligned(p, granularity) {
		return .Invalid_Pointer
	}
	if size <= 0 {
		// NOTE: This would actually work fine on Windows, but we'd need to track
		//       allocations on every other system.
		unimplemented("Page allocator does not track size. Try mem.free_with_size().")
	}
	size_full := mem.align_forward_int(size, granularity)
	virtual.release(p, uint(size_full))
	return nil
}

@(require_results)
page_allocator_make :: proc(granularity := GRANULARITY_MIN, flags: Page_Allocator_Flags = {})  -> Page_Allocator {
	assert(granularity >= GRANULARITY_MIN)
	assert(granularity <= GRANULARITY_MAX)
	assert(runtime.is_power_of_two(granularity))
	return Page_Allocator {
		flags = flags,
		granularity_log2 = u8(intrinsics.count_trailing_zeros(granularity)),
	}
}

// The Page_Allocator does not track individual allocations, so the struct itself
// only contains configuration. If the data is nil, we will just use the defaults.
@(require_results)
page_allocator :: proc(allocator: ^Page_Allocator = nil)-> mem.Allocator {
	return mem.Allocator {
		procedure = page_allocator_proc,
		data = allocator,
	}
}

page_allocator_proc :: proc(allocator_data: rawptr, mode: mem.Allocator_Mode,
			    size, alignment: int,
			    old_memory: rawptr, old_size: int, loc := #caller_location) -> ([]byte, mem.Allocator_Error) {
	flags: Page_Allocator_Flags
	granularity := GRANULARITY_MIN

	if allocator := (^Page_Allocator)(allocator_data); allocator != nil {
		flags = allocator.flags
		if allocator.granularity_log2 != 0 {
			granularity = 1 << allocator.granularity_log2
		}
	}

	switch mode {
	case .Alloc_Non_Zeroed:
		flags += {.Uninitialized_Memory}
		return page_aligned_alloc(size, alignment, granularity, flags)

	case .Alloc:
		flags -= {.Uninitialized_Memory}
		return page_aligned_alloc(size, alignment, granularity, flags)

	case .Free:
		return nil, page_free(old_memory, old_size, granularity, flags)

	case .Free_All:
		return nil, .Mode_Not_Implemented

	case .Resize_Non_Zeroed:
		flags += {.Uninitialized_Memory}
		break

	case .Resize:
		flags -= {.Uninitialized_Memory}
		break

	case .Query_Features:
		set := (^mem.Allocator_Mode_Set)(old_memory)
		if set != nil {
			set^ = {.Alloc, .Alloc_Non_Zeroed, .Free, .Resize, .Resize_Non_Zeroed, .Query_Features}
		}
		return nil, nil

	case .Query_Info:
		return nil, .Mode_Not_Implemented
	}

	// resizing
	if old_memory == nil {
		return page_aligned_alloc(size, alignment, granularity, flags)
	}
	if size == 0 {
		return nil, page_free(old_memory, old_size, granularity, flags)
	}
	return page_aligned_resize(old_memory, old_size, size, alignment, granularity, flags)
}

