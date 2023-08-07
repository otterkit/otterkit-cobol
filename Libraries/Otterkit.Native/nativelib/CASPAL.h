// Compiler, Architecture, Standard, and Platform Abstraction Layer
// for the Otterkit Native Library.

#ifndef CASPAL
#define CASPAL

// C Standard version detection
#if __STDC_VERSION__ >= 202311L
    #define C23OrLater
#elif __STDC_VERSION__ >= 201112L
    #define C11OrLater
#elif __STDC_VERSION__ >= 199901L
    #define C99OrLater
#else
    #error "C99-compliant (or later) compiler required."
#endif

// Compiler detection
#if defined(__clang__)
    #define CompilerClang
#elif defined(__GNUC__)
    #define CompilerGCC
#elif defined(_MSC_VER)
    #error "MSVC is not standard-compliant (C99 or later)."
#else
    #error "Unsupported compiler."
#endif

// Architecture detection
#if defined(__x86_64__)
    #define AMD64
#elif defined(__aarch64__)
    #define ARM64
#else
    #error "Unsupported architecture."
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



#endif // CASPAL