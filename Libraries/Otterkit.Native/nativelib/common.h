#if !defined(OTTERKIT_COMMON_H)
    #define OTTERKIT_COMMON_H
    #ifdef _WIN32
        #define public __declspec(dllexport)
    #else
        #define public __attribute__((visibility("default")))
    #endif

    #define internal static inline
    #define private static

    #define ref *
    #define deref *
    #define addr &
    #define null NULL

    typedef unsigned char uint8;
    typedef unsigned short uint16;
    typedef unsigned int uint32;
    typedef unsigned long long uint64;

    typedef signed char int8;
    typedef signed short int16;
    typedef signed int int32;
    typedef signed long long int64;

    #if defined(__amd64__)
        #if defined(_MSC_VER)
            #include <intrin.h>
        #else
            #include <x86intrin.h>
        #endif
    #endif

    #if defined(__aarch64__)
        #include <arm_neon.h>
    #endif

    typedef __m128i vec128i;
    typedef __m256i vec256i;

    // Memory alignment: 16 bytes.
    #define ALIGNMENT 16

#endif
