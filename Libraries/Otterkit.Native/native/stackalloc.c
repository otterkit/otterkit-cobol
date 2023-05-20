#include <stdlib.h>
#include <stdint.h>
#include <malloc.h>

#ifdef _WIN32
    #define _export __declspec(dllexport)

#else
    #define _export

#endif

// compile win-x64: cl.exe /O2 /LD /Fe:..\build\nativelib.dll stackalloc.c

// compile linux-x64: clang -shared -Wl,-rpath -O2 -fPIC -Wall -W -o ../build/nativelib.dylib stackalloc.c

// compile macos-x64: clang -dynamiclib -O2 -Wall -W -o ../build/nativelib.dylib stackalloc.c

// Default stack length: 512KB.
#define DEFAULT_LENGTH 524288

// Max stack length: 25MB.
#define MAX_LENGTH 26214400

typedef struct
{
    // Byte pointer to the allocated stack memory.
    uint8_t *pointer;
    // Length of the stack memory.
    size_t length;
    
} ottrstack_t;

_export ottrstack_t alloc(size_t length)
{
    // alloc(0) to use default length.
    // We don't want to allocate less than 512KB of stack memory.
    if (length < DEFAULT_LENGTH) length = DEFAULT_LENGTH;

    ottrstack_t stack = {NULL, 0};

    // Return a null stack, if memory requested is bigger than 25MB.
    // We don't want to attempt allocating more than 25MB of stack memory.
    if (length > MAX_LENGTH) return stack;

    #ifdef _WIN32
    uint8_t *pointer = _aligned_malloc(length, sizeof(uint8_t));

    #else
    uint8_t *pointer = aligned_alloc(sizeof(uint8_t), length);

    #endif

    stack.pointer = pointer;

    stack.length = length;

    return stack;
}

_export void dealloc(ottrstack_t stack)
{
    if (stack.pointer == NULL || stack.length == 0) return;
    
    #ifdef _WIN32
    _aligned_free(stack.pointer);
    
    #else
    free(stack.pointer);

    #endif
}
