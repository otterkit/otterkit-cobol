#include <string.h>

#include "allocator.h"
#include "common.h"

// Convert megabytes to bytes.
#define MB(x) ((uint32_t)(x) * 1024 * 1024)

// Convert kilobytes to bytes.
#define KB(x) ((uint32_t)(x) * 1024)

// Memory alignment, default: 8 bytes.
#define ALIGNMENT 8

// Stack memory, default: 2 MB.
static uint8_t Stack[MB(2)];

// Stack pointer, points to the next available memory address.
static uint8_t *StackPointer = Stack;

// Returns the amount of stack memory being used.
// Might be useful for debugging or profiling.
_export int32_t StackUsage()
{
    return StackPointer - Stack;
}

// Returns the amount of available stack memory.
// Might also be useful for debugging or profiling.
_export int32_t StackAvailable()
{
    return sizeof(Stack) - StackUsage();
}

// Allocate memory on the stack.
// Returns: 
// A pointer to the allocated memory,
// A NULL pointer if out of memory,
// The current stack pointer if length is zero.
_export void *Alloc(uint32_t length)
{
    // Return current stack pointer, if length is zero.
    if (length == 0) return StackPointer;

    // Return a null pointer, if there is not enough memory left on the stack.
    if (length > sizeof(Stack) - (StackPointer - Stack)) return NULL;

    // Align the length to the next multiple of ALIGNMENT bytes.
    length = ((length + ALIGNMENT) & -ALIGNMENT);

    // Get a pointer to the next available memory address.
    void *memory = StackPointer;
    
    // Increment stack pointer.
    StackPointer += length;

    return memory;
}

// Deallocate memory on the stack.
_export void Dealloc(void *memory)
{
    // Return, if memory is null.
    // Double free is not a good idea :)
    if (memory == NULL) return;

    // Reset stack pointer.
    // We don't need to zero out the memory, because it will be overwritten
    // when the next allocation is made.
    StackPointer = memory;
}
