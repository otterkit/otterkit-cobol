#if !defined(OTTERKIT_ALLOCATOR_H)
#define OTTERKIT_ALLOCATOR_H

#include <stdint.h>

// Returns the amount of stack memory being used.
// Might be useful for debugging or profiling.
int32_t StackUsage();

// Returns the amount of available stack memory.
// Might be useful for debugging or profiling.
int32_t StackAvailable();

// Allocate memory on the stack.
// Returns: 
// A pointer to the allocated memory,
// A NULL pointer if out of memory,
// The current stack pointer if length is zero.
void *Alloc(uint32_t length);

// Deallocate memory on the stack.
void Dealloc(void *memory);

#endif // OTTERKIT_ALLOCATOR_H
