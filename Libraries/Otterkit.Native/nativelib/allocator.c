#include <stdlib.h>
#include <stdint.h>

#include "common.h"

// Max allocation length: 25MB.
#define MAX_LENGTH 26214400

typedef struct
{
    // Byte pointer to the allocated memory.
    uint8_t *pointer;
    // Length of the memory.
    int32_t length;
    
} ottrptr_t;

_export ottrptr_t alloc(int32_t length)
{
    ottrptr_t memory = {NULL, 0};

    // Return a null pointer, if memory requested is less than 1 byte.
    // We don't want to attempt allocating 0 bytes of memory.
    if (length < 1) return memory;

    // Return a null pointer, if memory requested is bigger than 25MB.
    // We don't want to attempt allocating more than 25MB of memory.
    if (length > MAX_LENGTH) return memory;

    uint8_t *pointer = malloc(length);

    memory.pointer = pointer;

    memory.length = length;

    return memory;
}

_export void dealloc(ottrptr_t memory)
{
    if (memory.pointer == NULL || memory.length == 0) return;

    free(memory.pointer);
}
