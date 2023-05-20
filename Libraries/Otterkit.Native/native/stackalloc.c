#include <stdint.h>
#include <stdbool.h>

#ifdef _WIN32
    #define _export __declspec(dllexport)

    #include <heapapi.h> // HeapAlloc
    #include <sysinfoapi.h> // page size

    #define PAGESIZE 
#else
    #define _export

    #include <sys/mman.h> // mmap
    #include <unistd.h> // page size

    #define PAGESIZE sysconf(_SC_PAGESIZE)
#endif

// Default stack length: 512KB.
#define DEFAULT_LENGTH 524288

typedef struct
{
    // Pointer to the allocated stack memory.
    uint8_t *pointer;
    // Length of the stack memory.
    size_t length;
    // Is the stack currently allocated?
    bool allocated;
} ottrstack_t;

// Align stack memory to page size.
_export size_t align(size_t length) 
{
    #ifdef _WIN32
    // TODO Windows page size alignment...
    return 0;
    #else
    // Unix page size aligment
    return (length + PAGESIZE - 1) & ~(PAGESIZE - 1);

    #endif
}

_export ottrstack_t alloc(size_t length)
{
    if (length <= 0UL) length = align(DEFAULT_LENGTH);
    
    if (length >= 1UL) length = align(length);

    ottrstack_t stack = {NULL, 0UL, false};

    #ifdef _WIN32
    // TODO Windows memory allocation
    void *pointer = NULL;
    #else
    // Unix memory allocation
    void *pointer = mmap(NULL, length, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (pointer == MAP_FAILED)
    {
        // Return empty stack.
        return stack;
    }
    #endif

    stack.pointer = pointer;

    stack.length = length;

    stack.allocated = true;

    return stack;
}

_export void dealloc(ottrstack_t *stack)
{
    #ifdef _WIN32
    // TODO Windows memory deallocation
    #else
    // Unix memory deallocation
    munmap(stack->pointer, stack->length);
    #endif

    stack->allocated = false;
}
