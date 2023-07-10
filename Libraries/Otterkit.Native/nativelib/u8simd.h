#include <string.h>
#include <stdint.h>

#if !defined(OTTERKIT_U8SIMD_H)
#define OTTERKIT_U8SIMD_H

#if defined(__AVX2__) || defined(__SSE4_1__)
    #if defined(_MSC_VER)
        #include <intrin.h>
    #else
        #include <x86intrin.h>
    #endif
#endif

#if defined(__NEON__)
    #include <arm_neon.h>
#endif

#endif // OTTERKIT_U8SIMD_H
