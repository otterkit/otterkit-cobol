// Compiler, Architecture, Standard, and Platform Abstraction Layer
// for the Otterkit Runtime Library.

#ifndef CASPAL
#define CASPAL

#if defined(__cplusplus)
    #error "This header is meant to be used in C code only."
#endif

// C Standard version detection
#if __STDC_VERSION__ >= 202311L
    #define C23OrLater
#elif __STDC_VERSION__ >= 201112L
    #define C11OrLater
#elif __STDC_VERSION__ >= 199901L
    #define C99OrLater
#else
    #error "Standard C99 (or later) compliant compiler required."
#endif

// As defined in the C11 standard:
// "An integer constant expression with the value 0, or such an expression cast to type void *, is called a null pointer constant".
// We're calling it nullptr for the sake of clarity, and also uppercase NULL looks ugly (sorry, but it's true).
#define nullptr ((void *)0)

#if defined(C11OrLater)
    // C11 has a built-in static assert, nice.
    #define StaticAssert(condition, error) _Static_assert(condition, #error);
#else
    // C99 doesn't have a built-in static assert, so we have to use this neat trick.
    // If the condition is false, the compiler will complain that the array size is negative.
    #define StaticAssert(condition, error) typedef int assert_ ## error[(condition) ? 5 : -5];
#endif

#if defined(C23OrLater)
    // C23 has a built-in alignas, nice.
    #define aligned(x) alignas(x)
#elif defined(C11OrLater)
    // C11 has a built-in alignas, but it's not as nice as C23's.
    #define aligned(x) _Alignas(x)
#else
    // C99 doesn't have a built-in alignas, so we have to use compiler-specific attributes.
    #define aligned(x) __attribute__((aligned(x)))
#endif

// Platform detection
#if defined(_WIN64)
    #define PlatformWindows
#elif defined(__linux__)
    #define PlatformLinux
#elif defined(__APPLE__)
    #define PlatformApple
#else
    #error "Unsupported platform."
#endif

// Shared library visibility, export all "public" symbols.
#define public __attribute__((visibility("default")))

// For the sake of readability, since static is used for multiple things.
#define private static

// Architecture detection
#if defined(__x86_64__)
    #define AMD64
#elif defined(__aarch64__)
    #define ARM64
#else
    // We won't support any 32-bit architectures.
    #error "Unsupported architecture."
#endif

// Typedef unsigned integer types...
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef unsigned long long uint64;

// ...and make sure uint64 is actually 64 bits.
StaticAssert(sizeof(uint64) == 8, InvalidUint64Size);

// Typedef signed integer types...
typedef signed char int8;
typedef signed short int16;
typedef signed int int32;
typedef signed long long int64;

// ...and make sure int64 is actually 64 bits.
StaticAssert(sizeof(int64) == 8, InvalidInt64Size);

typedef uint64 uintptr;
typedef int64 intptr;

#if defined(AMD64)
    #if defined(PlatformWindows)
        #include <intrin.h>
    #else
        #include <x86intrin.h>
    #endif
#elif defined(ARM64)
    #include <arm_neon.h>
#endif

// Just to make things easier to read, double underscores everywhere is ugly and hard to read.
typedef __m128i vec128i;
typedef __m256i vec256i;

#endif // CASPAL