#include "CASPAL.h"

typedef struct str
{
	uint8* Memory;
	uint32 Length;
} String;

public void strAlignedZeroFill(const String to)
{
	// Goto loop buffer index.
	uint32 i = 0;

	// 128-bit vector of zeros.
    vec128i zeros = _mm_setzero_si128();

	// Loop through the remaining bytes in the destination buffer, zeroing them out.
	zeroLoop:
	// 128-bit store of zeros to destination.
	_mm_store_si128((vec128i*) to.Memory + i, zeros);

	i += 16;

	// If we haven't reached the end of the buffer, loop again.
	if (i < to.Length) goto zeroLoop;
}

public void strAlignedFill(const uint8 with, const String to)
{
	// Goto loop buffer index.
	uint32 i = 0;

	// 128-bit vector of the fill byte.
	vec128i fill = _mm_set1_epi8(with);

	// Loop through the remaining bytes in the destination buffer, zeroing them out.
	fillLoop:
	// 128-bit store to destination.
	_mm_store_si128((vec128i*) to.Memory + i, fill);

	i += 16;

	// If we haven't reached the end of the buffer, loop again.
	if (i < to.Length) goto fillLoop;
}

public void strAlignedMove(const String from, const String to)
{
	// Goto loop buffer index.
	uint32 i = 0;

	// Loop through the source and destination buffers, copying 16 bytes at a time.
	copyLoop:
	// 128-bit load from source, 128-bit store to destination.
	_mm_store_si128((vec128i*) to.Memory + i,
		_mm_load_si128((vec128i const*) from.Memory + i)
	);

	i += 16;

	// If we haven't reached the end of the buffer, loop again.
	if (i < from.Length && i < to.Length) goto copyLoop;

	// If we've reached the end of the destination buffer, no remaining bytes to zero out.
	if (i >= to.Length) return;

	// Loop through the remaining bytes in the destination buffer, zeroing them out.
	zeroLoop:
	// 128-bit store to destination.
	_mm_store_si128((vec128i*) to.Memory + i, _mm_setzero_si128());

	i += 16;

	// If we haven't reached the end of the buffer, loop again.
	if (i < to.Length) goto zeroLoop;
}
