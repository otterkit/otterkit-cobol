#include "allocator.h"

// Convert megabytes to bytes.
#define MB(x) ((uint32)(x) * 1024 * 1024)

// Convert kilobytes to bytes.
#define KB(x) ((uint32)(x) * 1024)

// Stack memory, default: 2 MB.
private uint8 Stack[MB(2)];

// Stack pointer, points to the next available memory address.
private uint8 ref StackPointer = Stack;

// Returns the amount of stack memory being used.
// Might be useful for debugging or profiling.
public int32 StackUsage()
{
    return StackPointer - Stack;
}

// Returns the amount of available stack memory.
// Might also be useful for debugging or profiling.
public int32 StackAvailable()
{
    return sizeof(Stack) - StackUsage();
}

// Allocate memory on the stack.
// Returns: 
// A pointer to the allocated memory,
// A NULL pointer if out of memory,
// The current stack pointer if length is zero.
public void ref Alloc(uint32 length)
{
    // Return current stack pointer, if length is zero.
    if (length == 0) return StackPointer;

    // Return a null pointer, if there is not enough memory left on the stack.
    if (length > sizeof(Stack) - (StackPointer - Stack)) return NULL;

    // Align the length to the next multiple of ALIGNMENT bytes.
    length = ((length + ALIGNMENT) & -ALIGNMENT);

    // Get a pointer to the next available memory address.
    void ref memory = StackPointer;
    
    // Increment stack pointer.
    StackPointer += length;

    return memory;
}

// Deallocate memory on the stack.
public void Dealloc(void ref memory)
{
    // Return if memory is null.
    // Double free is not a good idea :)
    if (memory == null) return;

    // Reset stack pointer.
    // We don't need to zero out the memory, because it will be overwritten
    // when the next allocation is made.
    StackPointer = memory;
}
