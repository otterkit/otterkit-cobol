#if !defined(OTTERKIT_COMMON_H)
    #define OTTERKIT_COMMON_H
    #ifdef _WIN32
        #define public __declspec(dllexport)
    #else
        #define public
    #endif

    #define internal static inline
    #define private static

    #define ref *
    #define deref *
    #define addr &
    #define null NULL

    #define is ==
    #define not !
    #define and &&
    #define or ||

    #include <stdint.h>

    typedef uint8_t u8;
    typedef uint16_t u16;
    typedef uint32_t u32;
    typedef uint64_t u64;

    typedef int8_t i8;
    typedef int16_t i16;
    typedef int32_t i32;
    typedef int64_t i64;

    typedef float f32;
    typedef double f64;
    
#endif
