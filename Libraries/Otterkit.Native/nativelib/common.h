
#ifndef _COMMON_H
    #define _COMMON_H
    #ifdef _WIN32
        #define _export __declspec(dllexport)
    #else
        #define _export
    #endif
#endif
