#include "CASPAL.h"

// Thin abstraction of a contiguous block of memory.
typedef struct span_t { uint8* pointer; uint64 length; } MemorySpan;

public void ottrkAlignedZeroFill(const MemorySpan to)
{
	// Goto loop buffer index.
	uint64 i = 0;

	// 128-bit vector of zeros.
    vec128i zeros = _mm_setzero_si128();

	// Loop through the remaining bytes in the destination buffer, zeroing them out.
	zeroLoop:
	// 128-bit store of zeros to destination.
	_mm_store_si128((vec128i*) (to.pointer + i), zeros);

	i += 16;

	// If we haven't reached the end of the buffer, loop again.
	if (i < to.length) goto zeroLoop;
}

public void ottrkAlignedFill(const uint8 with, const MemorySpan to)
{
	// Goto loop buffer index.
	uint64 i = 0;

	// 128-bit vector of the fill byte.
	vec128i fill = _mm_set1_epi8(with);

	// Loop through the remaining bytes in the destination buffer, zeroing them out.
	fillLoop:
	// 128-bit store to destination.
	_mm_store_si128((vec128i*) to.pointer + i, fill);

	i += 16;

	// If we haven't reached the end of the buffer, loop again.
	if (i < to.length) goto fillLoop;
}

public void ottrkAlignedMove(const MemorySpan from, const MemorySpan to)
{
	// Goto loop buffer index.
	uint64 i = 0;

	// Loop through the source and destination buffers, copying 16 bytes at a time.
	copyLoop:
	// 128-bit load from source, 128-bit store to destination.
	_mm_store_si128((vec128i*) to.pointer + i,
		_mm_load_si128((vec128i const*) from.pointer + i)
	);

	i += 16;

	// If we haven't reached the end of the buffer, loop again.
	if (i < from.length && i < to.length) goto copyLoop;

	// If we've reached the end of the destination buffer, no remaining bytes to zero out.
	if (i >= to.length) return;

	// Loop through the remaining bytes in the destination buffer, zeroing them out.
	zeroLoop:
	// 128-bit store to destination.
	_mm_store_si128((vec128i*) to.pointer + i, _mm_setzero_si128());

	i += 16;

	// If we haven't reached the end of the buffer, loop again.
	if (i < to.length) goto zeroLoop;
}
