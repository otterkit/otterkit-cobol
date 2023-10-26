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

#if defined C23OrLater 
    // Built-in Standard C23 static assert.
    #define StaticAssert(condition, error) static_assert(condition, error);
#elif defined C11OrLater 
    // Built-in Standard C11 static assert.
    #define StaticAssert(condition, error) _Static_assert(condition, error);
#else
    // C99 doesn't have a built-in static assert, so we have to use this neat trick.
    // If the condition is false, the compiler will complain that the array size is negative.
    #define StaticAssert(condition, error) typedef int Assert ## __LINE__ ## Static[(condition) ? 5 : -5];

#endif

#if defined C23OrLater 
    // Built-in Standard C23 alignas.
    #define aligned(x) alignas(x)
#elif defined C11OrLater 
    // Built-in Standard C11 alignas.
    #define aligned(x) _Alignas(x)
#elif defined C99OrLater && (defined __GNUC__  || defined __clang__) 
    // C99 doesn't have a built-in alignas, so we use compiler-specific attributes.
    #define aligned(x) __attribute__((aligned(x)))
#else
    // No alignas support, just ignore it.
    #define aligned(x)
#endif

#if defined C23OrLater
    // C23 already defines bool (equivalent to _Bool), true, and false as language keywords.
    // There's (most likely) no need for us to do anything here.
    // According to the C23 standard, when casting to a bool, the result is false if the 
    // value is a zero (for arithmetic types) or null (for pointers), otherwise the result is true.
#else
    // C99 introducted _Bool, we'll typedef it as bool for the sake of readability.
    // According to the C99 and C11 standards, when casting to a _Bool the result
    // is 0 if the value compares equal to 0, otherwise the result is 1.
    typedef _Bool bool;
    #define true  ((bool)1)
    #define false ((bool)0)
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
StaticAssert(sizeof(uint8) == 1, "Invalid uint8 size")
StaticAssert(sizeof(uint16) == 2, "Invalid uint16 size")
StaticAssert(sizeof(uint32) == 4, "Invalid uint32 size")
StaticAssert(sizeof(uint64) == 8, "Invalid uint64 size")

// Typedef signed integer types...
typedef signed char int8;
typedef signed short int16;
typedef signed int int32;
typedef signed long long int64;

// ...and make sure they are actually the correct size.
StaticAssert(sizeof(int8) == 1, "Invalid int8 size")
StaticAssert(sizeof(int16) == 2, "Invalid int16 size")
StaticAssert(sizeof(int32) == 4, "Invalid int32 size")
StaticAssert(sizeof(int64) == 8, "Invalid int64 size")

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

#define assembly __asm__

//╭──────────────────────────────────────────────────────────────────────────────────╮
//│  Platform detection and abstractions                                             │
//╰──────────────────────────────────────────────────────────────────────────────────╯ 

#if defined _WIN64
    #define PlatformWindows
    // We need to include it, otherwise it won't compile (Why though?)
    #include <windows.h>

#elif defined __linux__
    // I hope we don't need to check for individual distributions, that would be a pain.
    #define PlatformLinux

#elif defined __APPLE__
    // Darwin is the name of the kernel used by both macOS and iOS (and others).
    #define PlatformDarwin

#else
    #error "Unrecognized or unsupported platform."
#endif

//╭──────────────────────────────────────────────────────────────────────────────────╮
//│  Platform dependent virtual memory wrappers and functions                        │
//╰──────────────────────────────────────────────────────────────────────────────────╯ 

#if defined PlatformWindows
    #include <memoryapi.h>

    #define SYS_READWRITE PAGE_READWRITE
    #define SYS_PROTECTED PAGE_NOACCESS

    // On Windows, attempting to reserve an address that's already reserved will fail.
    // This is contrary to mmap's behavior, which will just overwrite the existing mapping.
    #define SYS_ALLOCATE MEM_COMMIT | MEM_RESERVE
    #define SYS_RESERVE MEM_RESERVE

    // Wish we had these on Unix, but we don't. Would make the intent clearer.
    #define SYS_COMMIT MEM_COMMIT
    #define SYS_DECOMMIT MEM_DECOMMIT

    // Only really needed on Windows, but we define it anyway for consistency.
    #define SYS_RELEASE MEM_RELEASE

    // Requests more virtual memory from the operating system (Windows Edition).
    #define SystemAlloc(addr, size, prot, flags) VirtualAlloc(addr, size, flags, prot)

    // This also releases the address space, so it shouldn't be used to decommit virtual memory.
    // Use both SystemCommit and SystemDecommit for that instead.
    #define SystemDealloc(addr, size) VirtualFree(addr, size, SYS_RELEASE)

    // Must be used with an address within a reserved address space (returned by SystemAlloc).
    #define SystemCommit(addr, size) VirtualAlloc(addr, size, SYS_COMMIT, SYS_READWRITE)

    // On Windows, we decommit (only release physical memory) by calling VirtualFree with MEM_DECOMMIT.
    // (according to the documentation, this is the correct way to do it)
    #define SystemDecommit(addr, size) VirtualFree(addr, size, SYS_DECOMMIT)
#elif defined PlatformLinux || defined PlatformDarwin
    #include <sys/mman.h>
    
    #define SYS_READWRITE PROT_READ | PROT_WRITE
    #define SYS_PROTECTED PROT_NONE

    // These 2 have duplicate flags, but it's easier to maintain this way.
    // This makes the intent of the code using them clearer, and more portable.
    #define SYS_ALLOCATE MAP_PRIVATE | MAP_ANONYMOUS
    #define SYS_RESERVE MAP_PRIVATE | MAP_ANONYMOUS

    // These 2 also have duplicate flags, same reason as above.
    #define SYS_COMMIT MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS
    #define SYS_DECOMMIT MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS

    // Not needed on Linux and macOS, but we define it anyway for consistency.
    #define SYS_RELEASE 0

    // Requests more virtual memory from the operating system (Unix Edition).
    #define SystemAlloc(addr, size, prot, flags) mmap(addr, size, prot, flags, -1, 0)

    // This also releases the address space, so it shouldn't be used to decommit virtual memory.
    // Use both SystemCommit and SystemDecommit for that instead.
    #define SystemDealloc(addr, size) munmap(addr, size)

    // Must be used with an address within a reserved address space (returned by SystemAlloc).
    #define SystemCommit(addr, size) mmap(addr, size, SYS_READWRITE, SYS_COMMIT, -1, 0)
    
    // On Linux and macOS, we decommit (only release physical memory) by calling mmap with PROT_NONE.
    // This will overwrite the existing mapping, and the pages will be physically released.
    #define SystemDecommit(addr, size) mmap(addr, size, SYS_PROTECTED, SYS_DECOMMIT, -1, 0)
#endif

//╭──────────────────────────────────────────────────────────────────────────────────╮
//│  Additional convenience macros and wrappers                                      │
//╰──────────────────────────────────────────────────────────────────────────────────╯ 

// Shared library, set visibility to export all "public" symbols.
// Should be used with `-fvisibility=hidden` compiler flag on GCC and Clang.
#if (defined __GNUC__ || defined __clang__) && defined PlatformWindows
    // `visibility("default")` doesn't seem to work on Windows. Fortunately both compilers
    // support the below attribute as well, which doesn't require the `__declspec` MSVC syntax.
    #define public __attribute__((dllexport))
#elif (defined __GNUC__ || defined __clang__)
    // This should work on most Unix-like systems. It should also be used together with 
    // `-fvisibility=hidden` to avoid bloating the export table with unnecessary symbols.
    #define public __attribute__((visibility("default")))
#else
    // Not supported on other compilers, just ignore it.
    #define public
#endif

// Shared library initializer and finalizer attributes.
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

// Branch prediction hints for performance optimizations.
#if defined __GNUC__ || defined __clang__
    // According to the C99, C11 and C23 standards, casting to a bool here should be safe.
    #define likely(expect, condition) (__builtin_expect((bool)(condition), (bool)(expect)))
#else
    // Not supported on other compilers, just ignore it.
    #define likely(expect, condition) (condition)
#endif

#if (defined __GNUC__ || defined __clang__) && !defined PlatformDarwin
    #define alias(name) __attribute__((alias(#name), copy(name), visibility("default"), used));
#endif

// For the sake of readability, since static is used for multiple different things.
#define private static

#endif // CASPAL
