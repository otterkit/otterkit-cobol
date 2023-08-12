#include "CASPAL.h"

#if defined(PlatformWindows)
    #include <memoryapi.h>
#else
    #include <sys/mman.h>
#endif

inline void* sysVirtualAlloc(uint64 size)
{
    #if PlatformWindows
        return VirtualAlloc(0, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    #else
        return mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    #endif
}

inline void sysVirtualDealloc(void* pointer, uint64 size)
{
    #if PlatformWindows
        VirtualFree(pointer, 0, MEM_RELEASE);
    #else
        munmap(pointer, size);
    #endif
}

inline void sysMemoryProtect(void* pointer, uint64 size, uint32 flags)
{
    #if PlatformWindows
        DWORD oldFlags;
        VirtualProtect(pointer, size, flags, &oldFlags);
    #else
        mprotect(pointer, size, flags);
    #endif
}


