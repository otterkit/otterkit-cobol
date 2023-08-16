// Compiler, Architecture, Standard, and Platform Abstraction Layer
// for the Otterkit Runtime Library.

#ifndef CASPAL
#define CASPAL 202300UL

#if defined(__cplusplus)
    #error "This header is meant to be used in C source files only."
#endif

// C Standard version detection and abstraction
#if __STDC_VERSION__ >= 202311L
    #define C23OrLater
#elif __STDC_VERSION__ >= 201112L
    #define C11OrLater
#elif __STDC_VERSION__ >= 199901L
    #define C99OrLater
#else
    // This includes MSVC, which still doesn't support any Standard C versions (Microsoft, please fix this!).
    #error "Standard C99 (or later) is required. Consider upgrading your compiler, or using a different one."
#endif

#if defined(C11OrLater)
    // C11 has a built-in static assert, nice!
    #define StaticAssert(condition, error) _Static_assert(condition, #error);
#else
    // C99 doesn't have a built-in static assert, so we have to use this neat trick.
    // If the condition is false, the compiler will complain that the array size is negative.
    #define StaticAssert(condition, error) typedef int assert_ ## __LINE__ ## error[(condition) ? 5 : -5];
#endif

#if defined(C23OrLater)
    // C23 has a built-in alignas, nice!
    #define aligned(x) alignas(x)
#elif defined(C11OrLater)
    // C11 has a built-in alignas, but it's not as nice as C23's.
    #define aligned(x) _Alignas(x)
#else
    // C99 doesn't have a built-in alignas, so we have to use compiler-specific attributes.
    #define aligned(x) __attribute__((aligned(x)))
#endif

// Platform detection and abstraction
#if defined(_WIN64)
    #define PlatformWindows
    #include <windows.h>
#elif defined(__linux__)
    #define PlatformLinux
#elif defined(__APPLE__)
    #define PlatformApple
#else
    #error "Unsupported platform."
#endif

// Virtual memory management wrappers
#if defined(PlatformWindows)
    #include <memoryapi.h>

    #define sysVirtualAlloc(addr, size, prot, flags) VirtualAlloc(addr, size, flags, prot)
    #define sysVirtualDealloc(addr, size, flags) VirtualFree(addr, size, flags)

    #define memReadWrite PAGE_READWRITE
    #define memProtected PAGE_NOACCESS

    #define memReserve MEM_RESERVE
    #define memCommit MEM_COMMIT
#elif defined(PlatformLinux) || defined(PlatformApple)
    #include <sys/mman.h>

    #define sysVirtualAlloc(addr, size, prot, flags) mmap(addr, size, prot, flags, -1, 0)
    #define sysVirtualDealloc(addr, size, flags) munmap(addr, size)

    #define memReadWrite PROT_READ | PROT_WRITE
    #define memProtected PROT_NONE

    #define memReserve MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS
    #define memCommit MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS
#endif

// Architecture detection and abstraction
#if defined(__x86_64__) || defined(_M_X64)
    #define AMD64
#elif defined(__aarch64__) || defined(_M_ARM64)
    #define ARM64
#else
    // We don't support 32-bit architectures.
    #error "Unsupported architecture."
#endif

// Typedef unsigned integer types...
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef unsigned long long uint64;

// ...and make sure they are actually the correct size.
StaticAssert(sizeof(uint8) == 1, InvalidUint8Size);
StaticAssert(sizeof(uint16) == 2, InvalidUint16Size);
StaticAssert(sizeof(uint32) == 4, InvalidUint32Size);
StaticAssert(sizeof(uint64) == 8, InvalidUint64Size);

// Typedef signed integer types...
typedef signed char int8;
typedef signed short int16;
typedef signed int int32;
typedef signed long long int64;

// ...and make sure they are actually the correct size.
StaticAssert(sizeof(int8) == 1, InvalidInt8Size);
StaticAssert(sizeof(int16) == 2, InvalidInt16Size);
StaticAssert(sizeof(int32) == 4, InvalidInt32Size);
StaticAssert(sizeof(int64) == 8, InvalidInt64Size);

// Sizes were already checked above.
typedef uint64 uintptr;
typedef int64 intptr;

// As defined in the C11 standard:
// "An integer constant expression with the value 0, or such an expression cast to type void *, is called a null pointer constant".
// We're calling it nullptr for the sake of clarity, and also because uppercase NULL looks ugly (sorry, but it's true).
#define nullptr ((void*)0)

// SIMD intrinsics for x86 and ARM.
#if defined(AMD64)
    #include <immintrin.h>
#elif defined(ARM64)
    #include <arm_neon.h>
#endif

// Just to make things easier to read, double underscores everywhere is ugly and hard to read.
typedef __m128i vec128i;
typedef __m256i vec256i;

// Shared library visibility, export all "public" symbols.
#define public __attribute__((visibility("default")))

// For the sake of readability, since static is used for multiple things.
#define private static

#endif // CASPAL