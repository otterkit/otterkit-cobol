#include "common.h"

typedef struct
{
    uint8 ref Memory;
    uint32 Length;
	uint32 Dynamic;
} Alphanumeric;

typedef struct str
{
	uint8* Memory;
	uint32 Length;
} String;


// Aligns an integer value to 16 bytes.
#define ALIGN(x) (((uint32)(x) + ALIGNMENT) & -ALIGNMENT)

public void AlphanumericZero(Alphanumeric ref destination)
{
	// Align the buffer length to 16 bytes.
	// Allocations are always 16-byte aligned, so this is safe.
	uint32 length = ALIGN(destination->Length);

	uint8 ref memory = destination->Memory;

	uint32 i = 0;

	vec128i zero = _mm_setzero_si128();

	while (i < length)
	{
		// 128-bit store to destination.
		_mm_store_si128((vec128i ref) memory + i, zero);

		i += 16;
	}
}

public void strAlignedMove(const String from, const String to)
{
	uint32 i = 0;

	// Loop through the source and destination buffers, copying 16 bytes at a time.
	copyLoop:
	// 128-bit load from source, 128-bit store to destination.
	_mm_store_si128((vec128i*) to.Memory + i,
		_mm_load_si128((vec128i const*) from.Memory + i)
	);

	i += 16;

	if (i < from.Length && i < to.Length) goto copyLoop;

	if (i >= to.Length) return;

	// Loop through the remaining bytes in the destination buffer, zeroing them out.
	zeroLoop:
	// 128-bit store to destination.
	_mm_store_si128((vec128i*) to.Memory + i, _mm_setzero_si128());

	i += 16;

	if (i < to.Length) goto zeroLoop;
}

public void AlphanumericCopy(Alphanumeric const ref source, Alphanumeric ref destination)
{
	// Align the source and destination lengths to 16 bytes.
	// Allocations are always 16-byte aligned, so this is safe.
	uint32 sLength = ALIGN(source->Length);
	uint32 dLength = ALIGN(destination->Length);

	uint8 ref sMemory = source->Memory;
	uint8 ref dMemory = destination->Memory;

	uint32 i = 0;

	// Loop through the source and destination buffers, copying 16 bytes at a time.
	while (i < sLength && i < dLength)
	{
		// 128-bit load from source, 128-bit store to destination.
		_mm_store_si128(
			(vec128i ref) dMemory + i,
			_mm_load_si128((vec128i const ref) sMemory + i)
		);

		i += 16;
	}

	// If the destination buffer is longer than the source buffer, zero out the remaining bytes.
	vec128i zero = _mm_setzero_si128();

	while (i < dLength)
	{
		// 128-bit store to destination.
		_mm_store_si128((vec128i ref) dMemory + i, zero);

		i += 16;
	}
}
