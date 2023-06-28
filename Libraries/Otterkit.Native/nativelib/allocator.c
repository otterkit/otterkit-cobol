#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "common.h"

// Convert megabytes to bytes.
#define MB(x) (x * 1024 * 1024)

// Convert kilobytes to bytes.
#define KB(x) (x * 1024)

// Stack memory, default size: 128 KB.
static const uint8_t Stack[KB(128)];

// Stack pointer, points to the next available memory address.
static uint8_t *StackPointer = (uint8_t *)Stack;

// Might be useful for debugging or profiling.
_export int32_t GetStackUsage()
{
    // Return the amount of memory used.
    return StackPointer - Stack;
}

// Might also be useful for debugging or profiling.
_export int32_t GetStackFree()
{
    // Return the amount of available memory.
    return sizeof(Stack) - GetStackUsage();
}

_export uint8_t *Alloc(int32_t length)
{
    // Return a null pointer, if memory requested is less than 1 byte.
    // We don't want to attempt allocating 0 bytes of memory.
    if (length < 1) return NULL;

    // Return a null pointer, if there is not enough memory left on the stack.
    if (StackPointer + length > Stack + sizeof(Stack)) return NULL;

    // Allocate memory.
    uint8_t *memory = StackPointer;
    
    // Increment stack pointer.
    StackPointer += length;

    return memory;
}

_export void Dealloc(uint8_t *memory)
{
    // Return, if memory is null.
    // Double free is not a good idea :)
    if (memory == NULL) return;

    // Reset stack pointer.
    // This frees all memory allocated after the given memory address.
    // We don't need to zero out the memory, because it will be overwritten
    // when the next allocation is made. It's a waste of precious CPU cycles.
    StackPointer = memory;
}
