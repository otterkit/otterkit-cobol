
#if !defined(__OTTERKIT_COMMON_H__)
    #define __OTTERKIT_COMMON_H__
    #ifdef _WIN32
        #define _export __declspec(dllexport)
    #else
        #define _export
    #endif
#endif
