#include "allocator.h"

// Convert terabytes to bytes.
#define TB(x) ((uint64)(x) * 1024 * 1024 * 1024 * 1024)

private inline void* ottrkReserveAddressSpace(const uint64 size)
{
    // Reserve virtual address space, the default size should be 2 TB when calling this function.
    #if defined(PlatformWindows)
        return sysVirtualAlloc(nullptr, size, memProtected, MEM_RESERVE);

    #elif defined(PlatformLinux) || defined(PlatformApple)
        return sysVirtualAlloc(nullptr, size, memProtected, MAP_NORESERVE);

    #endif
}

// TODO: Replace this with the proper allocator.

// Convert megabytes to bytes.
#define MB(x) ((uint32)(x) * 1024 * 1024)

// Convert kilobytes to bytes.
#define KB(x) ((uint32)(x) * 1024)

// Stack memory, default: 2 MB.
private uint8 Stack[MB(2)];

// Stack pointer, points to the next available memory address.
private uint8* StackPointer = Stack;

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
public void* Alloc(uint32 length)
{
    // Return current stack pointer, if length is zero.
    if (length == 0) return StackPointer;

    // Return a null pointer, if there is not enough memory left on the stack.
    if (length > sizeof(Stack) - (StackPointer - Stack)) return NULL;

    // Align the length to the next multiple of 16 bytes.
    length = ((length + 16) & -16);

    // Get a pointer to the next available memory address.
    void* memory = StackPointer;
    
    // Increment stack pointer.
    StackPointer += length;

    return memory;
}

// Deallocate memory on the stack.
public void Dealloc(void* memory)
{
    // Return if memory is null.
    // Double free is not a good idea :)
    if (memory == nullptr) return;

    // Reset stack pointer.
    // We don't need to zero out the memory, because it will be overwritten
    // when the next allocation is made.
    StackPointer = memory;
}
