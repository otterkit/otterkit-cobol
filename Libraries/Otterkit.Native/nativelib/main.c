#include <stdio.h>
#include "CASPAL.h"

// Convert terabytes to bytes.
#define TB(x) ((uint64)(x) * 1024 * 1024 * 1024 * 1024)

// Convert gigabytes to bytes.
#define GB(x) ((uint64)(x) * 1024 * 1024 * 1024)

// Convert megabytes to bytes.
#define MB(x) ((uint32)(x) * 1024 * 1024)

private inline void* ottrkReserveAddressSpace(const uint64 size)
{
    // Reserve virtual address space, the default size should be 2 TB when calling this function.
    #if defined(PlatformWindows)
        return sysVirtualAlloc(nullptr, size, memProtected, MEM_RESERVE);

    #elif defined(PlatformLinux) || defined(PlatformApple)
        return sysVirtualAlloc(nullptr, size, memProtected, MAP_NORESERVE | MAP_ANONYMOUS | MAP_PRIVATE);

    #endif
}

int main()
{
    printf("Press enter to reserve address space...");
    getchar();

    // Reserve 2 TB of virtual address space.
    void* addressSpace = ottrkReserveAddressSpace(TB(2));

    printf("Address space reserved at %p\n", addressSpace);

    if (addressSpace == nullptr)
    {
        printf("Failed to reserve address space.\n");
        return 1;
    }

    printf("Press enter to allocate 400MBs...");
    getchar();

    // Allocate 400 MB of memory on the reserved address space.
    void* memory = sysVirtualAlloc(addressSpace, MB(400), memReadWrite, MEM_COMMIT);

    printf("Allocated memory at %p\n", memory);

    if (memory != addressSpace)
    {
        printf("Failed to allocate memory at the specified address.\n");
        printf("Last error: %lu\n", GetLastError());
        return 1;
    }

    if (memory == nullptr)
    {
        printf("Failed to allocate memory.\n");
        return 1;
    }

    printf("Press enter to force page allocations...");
    getchar();

    // Loop through the allocated memory and write to it.
    for (uint32 i = 0; i < MB(400); i += 4096)
    {
        ((uint8*)memory)[i] = 0x90;
    }

    printf("Press enter to exit...");
    getchar();

    // Deallocate the address space.
    sysVirtualDealloc(addressSpace, TB(2), MEM_RELEASE);
}
