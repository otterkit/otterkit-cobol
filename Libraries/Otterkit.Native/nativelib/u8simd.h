#include <string.h>
#include <stdint.h>

#if !defined(__OTTERKIT_U8SIMD_H__)
#define __OTTERKIT_U8SIMD_H__


#if defined(AVX2) || defined(__x86_64__)
    #if defined(_MSC_VER)
        #include <intrin.h>
    #else
        #include <x86intrin.h>
    #endif
#endif

#if defined(NEON)
    #include <arm_neon.h>
#endif

#endif // __OTTERKIT_U8SIMD_H__
