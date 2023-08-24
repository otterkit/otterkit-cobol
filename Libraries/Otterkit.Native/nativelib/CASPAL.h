//╭─────────────────────────────────────────────────────────────────────────────────╮
//│   Otterkit's Compiler, Architecture, Standard, and Platform Abstraction Layer   │
//│  ╭──────────╮ ╭──────────╮ ╭──────────╮ ╭──────────╮ ╭──────────╮ ╭─╮           │
//│  │ ╭────────╯ │ ╭──────╮ │ │ ╭────────╯ │ ╭──────╮ │ │ ╭──────╮ │ │ │           │
//│  │ │          │ │      │ │ │ │          │ │      │ │ │ │      │ │ │ │           │
//│  │ │          │ ╰──────╯ │ │ ╰────────╮ │ ╰──────╯ │ │ ╰──────╯ │ │ │           │
//│  │ │          │ ╭──────╮ │ ╰────────╮ │ │ ╭────────╯ │ ╭──────╮ │ │ │           │
//│  │ │          │ │      │ │          │ │ │ │          │ │      │ │ │ │           │
//│  │ ╰────────╮ │ │      │ │ ╭────────╯ │ │ │          │ │      │ │ │ ╰────────╮  │
//│  ╰──────────╯ ╰─╯      ╰─╯ ╰──────────╯ ╰─╯          ╰─╯      ╰─╯ ╰──────────╯  │
//╰─────────────────────────────────────────────────────────────────────────────────╯ 

#ifndef CASPAL
#define CASPAL 202300L

#if defined __cplusplus // Just error out and give up if we're being included in a C++ file.
    #error "CASPAL was designed to be used within (standard) C source files only."
#endif

//╭──────────────────────────────────────────────────────────────────────────────────╮
//│  Standard C detection and abstractions                                           │
//╰──────────────────────────────────────────────────────────────────────────────────╯ 

#if __STDC_VERSION__ >= 202311L
    #define C23OrLater
#endif

#if __STDC_VERSION__ >= 201112L
    #define C11OrLater
#endif

#if __STDC_VERSION__ >= 199901L
    #define C99OrLater
#endif

#if !defined __STDC_VERSION__
    // Will error on MSVC! It still seems to be stuck on C89, it doesn't define __STDC_VERSION__, and only runs on Windows.
    // (Not worth the preprocessor gymnastics and compiler-specific hacks to support it in my opinion!)
    #error "Standard C99 (or later) support is required. Consider upgrading your compiler, or using GCC or Clang instead."
#endif

#if defined C11OrLater 
    // Built-in Standard C11 static assert.
    #define StaticAssert(condition, error) _Static_assert(condition, #error);

#else
    // C99 doesn't have a built-in static assert, so we have to use this neat trick.
    // If the condition is false, the compiler will complain that the array size is negative.
    #define StaticAssert(condition, error) typedef int assert_ ## __LINE__ ## error[(condition) ? 5 : -5];

#endif

#if defined C23OrLater 
    // Built-in Standard C23 alignas.
    #define aligned(x) alignas(x)

#elif defined C11OrLater 
    // Built-in Standard C11 alignas.
    #define aligned(x) _Alignas(x)

#elif defined C99OrLater && defined __GNUC__  || defined __clang__ 
    // C99 doesn't have a built-in alignas, so we use compiler-specific attributes.
    #define aligned(x) __attribute__((aligned(x)))

#else
    #define aligned(x) // No alignas support, just ignore it.

#endif

// As defined by the C11 standard:
// "An integer constant expression with the value 0, or such an expression cast to type void *, is called a null pointer constant".
// We're calling it nullptr for the sake of clarity, and also because uppercase NULL looks ugly (sorry, but it's true).
#define nullptr ((void*)0)

//╭──────────────────────────────────────────────────────────────────────────────────╮
//│  Architecture detection and abstractions                                         │
//╰──────────────────────────────────────────────────────────────────────────────────╯ 

#if defined __x86_64__ || defined _M_X64
    #define AMD64

#elif defined __aarch64__ || defined _M_ARM64
    #define ARM64

#else
    // We don't support 32-bit architectures, would limit some optimizations too much.
    #error "Unrecognized or unsupported architecture."
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

// SIMD intrinsics for x86 and ARM.
#if defined AMD64
    #include <immintrin.h>

    // Just to make things easier to read, double underscores everywhere is ugly and hard to read.
    typedef __m128i vec128i;

#elif defined ARM64
    #include <arm_neon.h>

    // Same as above, but for Aarch64 with NEON.
    typedef int8x16_t vec128i;
    
#endif

//╭──────────────────────────────────────────────────────────────────────────────────╮
//│  Platform detection and abstractions                                             │
//╰──────────────────────────────────────────────────────────────────────────────────╯ 

#if defined _WIN64
    #define PlatformWindows
    // We need to include this, otherwise it won't compile.
    #include <windows.h>

#elif defined __linux__
    #define PlatformLinux

#elif defined __APPLE__
    #define PlatformDarwin

#else
    #error "Unrecognized or unsupported platform."
#endif

//╭──────────────────────────────────────────────────────────────────────────────────╮
//│  Platform dependent virtual memory wrappers and functions                        │
//╰──────────────────────────────────────────────────────────────────────────────────╯ 

#if defined PlatformWindows
    #include <memoryapi.h>

    // Windows virtual memory primitives.
    #define sysVirtualAlloc(addr, size, prot, flags) VirtualAlloc(addr, size, flags, prot)
    #define sysVirtualDealloc(addr, size, flags) VirtualFree(addr, size, flags)

    #define memReadWrite PAGE_READWRITE
    #define memProtected PAGE_NOACCESS

    #define memReserve MEM_RESERVE
    #define memCommit MEM_COMMIT

    #define memDecommit MEM_DECOMMIT
    #define memRelease MEM_RELEASE

#elif defined PlatformLinux || defined PlatformDarwin
    #include <sys/mman.h>

    // Linux and macOS virtual memory primitives.
    #define sysVirtualAlloc(addr, size, prot, flags) mmap(addr, size, prot, flags, -1, 0)
    #define sysVirtualDealloc(addr, size, flags) munmap(addr, size)

    #define memReadWrite PROT_READ | PROT_WRITE
    #define memProtected PROT_NONE

    // MAP_NORESERVE to avoid reserving swap space for reserved address space.
    #define memReserve MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS
    #define memCommit MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS

    // MAP_NORESERVE to avoid reserving swap space for decommitted pages.
    #define memDecommit MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS
    // Not needed on Unix systems.
    #define memRelease 0

#endif

//╭──────────────────────────────────────────────────────────────────────────────────╮
//│  Additional virtual memory convenience wrappers                                  │
//╰──────────────────────────────────────────────────────────────────────────────────╯ 

// So we don't have to remember the order of the arguments and flags.
#define sysReserveAddressSpace(size) sysVirtualAlloc(nullptr, size, memProtected, memReserve)
#define sysReleaseAddressSpace(addr, size) sysVirtualDealloc(addr, size, memRelease)

// Must be used with an address within a reserved address space (returned by sysReserveAddressSpace).
#define sysCommitMemory(addr, size) sysVirtualAlloc(addr, size, memReadWrite, memCommit)

#if defined PlatformWindows
    // On Windows, we decommit (only release physical memory) by calling VirtualFree with MEM_DECOMMIT.
    // (according to the documentation, this is the correct way to do it)
    #define sysDecommitMemory(addr, size) sysVirtualDealloc(addr, size, memDecommit)

#elif defined PlatformLinux || defined PlatformDarwin
    // On Linux and macOS, we decommit (only release physical memory) by calling mmap with PROT_NONE.
    // This will overwrite the existing mapping, and the pages will be physically released.
    #define sysDecommitMemory(addr, size) sysVirtualAlloc(addr, size, memProtected, memDecommit)

#endif

//╭──────────────────────────────────────────────────────────────────────────────────╮
//│  Additional convenience macros and wrappers                                      │
//╰──────────────────────────────────────────────────────────────────────────────────╯ 

// Shared library, set visibility to export all "public" symbols.
// Should be used with `-fvisibility=hidden` compiler flag on GCC and Clang.
#if defined __GNUC__ || defined __clang__
    #define public __attribute__((visibility("default")))

#else
    // Not supported on other compilers, just ignore it.
    #define public
    
#endif

// Shared library, initializer and finalizer attributes.
#if defined __GNUC__ || defined __clang__
    // Library initializer attribute, function will be called when the library is loaded.
    #define initializer __attribute__((constructor))
    // Library finalizer attribute, function will be called when the library is unloaded.
    #define finalizer __attribute__((destructor))
#else
    // Not supported on other compilers, just ignore it.
    #define initializer
    #define finalizer
#endif

// For the sake of readability, since static is used for multiple different things.
#define private static

#endif // CASPAL