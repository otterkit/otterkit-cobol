#include "CASPAL.h"
#include <stdio.h>

typedef struct vpool_t
{
    uint16 Capacity;
    uint16 Available;
    uint16 Initialized;
    uint16 BlockLength;
    uint8* NextBlock;
} VirtualPool;

typedef struct vcache_t
{
    // Immediate: directly available for allocation.
    VirtualPool* Immediate;

    // Cached: used when present, instead of creating a new pool.
    VirtualPool* Cached;
} VirtualCache;

// Bit tree of available virtual pools.
typedef struct vtree_t
{
    // 64 bits (1 bit per 2 TiB)
    uint64 Root;
    
    // 4096 bits (1 bit per 32 GiB)
    uint64 Trunks[1 << 6];

    // 131072 bits (1 bit per 1 GiB)
    uint64 Branches[1 << 12];

    // 4194304 bits (1 bit per 32 MiB)
    uint64 Twigs[1 << 18];

    // 1073741824 bits (1 bit per 128 KiB)
    uint64 Leaves[1 << 24];
} VirtualTree;

typedef struct vallocator_t
{
    // Allocator's base virtual address space.
    uint8* BaseAddress;

    // This instance's capacity, in bytes (max 128 TiB).
    uint64 Capacity;

    // This instance's current usage, in bytes.
    uint64 Usage;

    // This instance's immediate pool lookup table and pool cache.
    // At least one pool is always kept in the cache after initialization.
    VirtualCache HeapCache[120];

    // This instance's bit tree, used to track available virtual pools.
    VirtualTree* BitTree;

    uint8 IsInitialized;
} VirtualAllocator;

static VirtualAllocator GlobalAllocator;

void VAllocInitialize(uint64 size)
{
    // Avoid overwriting the allocator if it has already been initialized.
    if (GlobalAllocator.IsInitialized) return;

    // If the size is 0, then we allocate the default 2TiB of address space.
    if (size == 0) size = 2ull << 40;

    // Reserve the heap's address space, and an additional 128KiB for alignment purposes.
    void* heap = sysReserveAddressSpace(size + 131072);

    // (void*)-1 is MAP_FAILED, expanded to avoid the check breaking on platforms where it is not defined.
    if (heap == nullptr || heap == (void*)-1)
    {
        fputs("Failed to reserve address space for the heap allocator.\n", stderr);
        exit(1);
    }

    // Commit the first 2MiB of the heap's address space.
    sysCommitMemory(heap, 2097152);

    // Align the heap to a 128KiB boundary (needed to make deallocation work properly)
    uintptr aligned = ((uintptr)heap + (131072 - 1)) & -131072;

    GlobalAllocator.BaseAddress = (uint8*)aligned;

    // Allocate 4MiB of memory for the virtual allocator header.
    uint8* headerAddress = sysVirtualAlloc(nullptr, 4194304, memReadWrite, memAllocate);

    uint64 index = 0;
    uint64 segmentSize = size / 16;
    
    // Initialize all 16 virtual segments.
    while (GlobalAllocator.Segments[15] == nullptr)
    {
        VirtualSegment** segment = &GlobalAllocator.Segments[index];

        *segment = (VirtualSegment*)(headerAddress + 163840ull * index);

        (*segment)->Memory = headerAddress + segmentSize * index;

        (*segment)->Available = segmentSize;
        (*segment)->Capacity = segmentSize;

        index++;
    }
}

// initializer void virtAllocatorInitializeDefault() { virtAllocatorInitialize(0); }

VirtualAllocator* VAllocFetchGlobalInstance() { return &GlobalAllocator; }

static void* FetchAvailableAddress(VirtualAllocator* instance)
{
    VirtualTree* tree = instance->BitTree;

    uint64* root = &tree->Root;

    uint64 rootIndex = __tzcnt_u64(~(*root)); // 64

    uint64* trunk = &tree->Trunks[rootIndex];

    uint64 trunkIndex = __tzcnt_u64(~(*trunk)); // 4096

    uint64* branch = &tree->Branches[
        (rootIndex << 6) + trunkIndex
    ];

    uint64 branchIndex = __tzcnt_u64(~(*branch)); // 131072

    uint64* twig = &tree->Twigs[
        (rootIndex << 12) + (trunkIndex << 6) + branchIndex
    ];

    uint64 twigIndex = __tzcnt_u64(~(*twig)); // 4194304

    uint64* leaf = &tree->Leaves[
        (rootIndex << 18) + (trunkIndex << 12) + (branchIndex << 6) + twigIndex
    ];

    uint64 leafIndex = __tzcnt_u64(~(*leaf)); // 1073741824

    *leaf |= (1u << leafIndex);

    if (*leaf == ~0u) *branch |= (1u << branchIndex);

    if (*branch == ~0u) *trunk |= (1u << trunkIndex);

    if (*trunk == ~0u) *root |= (1u << rootIndex);

    // If the root is full, then the entire segment is full (128GiB)
    // if (*root == ~0u) return nullptr;

    return (
        instance->BaseAddress
        + (rootIndex << 24)
        + (trunkIndex << 18)
        + (branchIndex << 12)
        + (twigIndex << 6)
        + leafIndex
    );
}

// void virtPoolRelease(VirtualSegment* segment, VirtualPool* pool)
// {
//     uint32* root = &segment->Root;

//     uint32 rootIndex = (
//         ((uintptr)pool - (uintptr)segment->Memory) >> 15
//     );

//     uint32* trunk = &segment->Trunks[
//         rootIndex
//     ];

//     uint32 trunkIndex = (
//         ((uintptr)pool - (uintptr)segment->Memory) >> 10
//     ) & ((1 << 5) - 1);

//     uint32* branch = &segment->Branches[
//         (rootIndex << 5) + trunkIndex
//     ];

//     uint32 branchIndex = (
//         ((uintptr)pool - (uintptr)segment->Memory) >> 5
//     ) & ((1 << 10) - 1);

//     uint32* leaf = &segment->Leaves[
//         (rootIndex << 10) + (trunkIndex << 5) + branchIndex
//     ];

//     uint32 leafIndex = (
//         ((uintptr)pool - (uintptr)segment->Memory)
//     ) & ((1 << 15) - 1);

//     *leaf &= ~(1u << leafIndex);

//     if (*leaf == 0u) *branch &= ~(1u << branchIndex);

//     if (*branch == 0u) *trunk &= ~(1u << trunkIndex);

//     if (*trunk == 0u) *root &= ~(1u << rootIndex);

//     // Decommit the pool's pages (128KiB), still keep it reserved.
//     sysDecommitMemory(pool, 131072);
// }

// Apparently, GCC can't optimize this function properly and generates a bunch of branches.
// This is a performance-critical function that is called every time a block is allocated.
// We can't afford to have it be slow due to branch mispredictions.
AssemblyFunction(uint64, AlignLengthToPool, uint64 length)

#if defined AMD64
Assembly (
    UsingIntelSyntax
    "AlignLengthToPool:         \n"
    "test    rcx, rcx           \n"
    "mov     rdx, 1             \n"
    "cmovne  rdx, rcx           \n"
    "lea     rcx, [rdx + 127]   \n"
    "lea     rax, [rdx + 15]    \n"
    "shr     rcx, 7             \n"
    "shr     rax, 4             \n"
    "add     rcx, 56            \n"
    "cmp     rdx, 1025          \n"
    "cmovae  rax, rcx           \n"
    "dec     rax                \n"
    "ret                        \n"
);
#elif defined ARM64
Assembly (
    "AlignLengthToPool:         \n"
    "cmp    x0,  #0             \n"
    "csinc  x8,  x0,  xzr, ne   \n"
    "add    x9,  x8,  #127      \n"
    "add    x10, x8,  #15       \n"
    "lsr    x9,  x9,  #7        \n"
    "lsr    x10, x10, #4        \n"
    "add    x9,  x9,  #56       \n"
    "cmp    x8,  #1025          \n"
    "csel   x8,  x10, x9,  lo   \n"
    "sub    x0,  x8,  #1        \n"
    "ret                        \n"
);
#endif

int main(void)
{
    for (uint64 i = 0; i < 8192; i += 16)
    {
        uint64 class = AlignLengthToClass(i);

        printf("Size: %llu, Class: %llu\n", i, class);
    }

    return 0;
}

// void* ottrkPoolAlloc(VirtualPool* pool)
// {
//     // If there are any uninitialized blocks, initialize one.
//     if (pool->Initialized < pool->Capacity)
//     {
//         // Get the address of the next uninitialized block.
//         uint8* uninitialized = (
//             (uint8*)pool + 32 + pool->Initialized * pool->BlockLength
//         );
        
//         // Set the next block pointer to the next available block.
//         *(uint8**)uninitialized = uninitialized + pool->BlockLength;

//         pool->Initialized++;
//     }

//     uint8* block = nullptr;

//     // If there are any available blocks, allocate one.
//     if (pool->Available)
//     {
//         block = pool->NextBlock;
//         pool->Available--;

//         // If there are any available blocks left, set the next block pointer to the next available block.
//         pool->NextBlock = (pool->Available) ? *(uint8**)block : nullptr;
//     }

//     return block;
// }

// void ottrkPoolDealloc(VirtualPool* pool, void* address)
// {
//     // Set the next block pointer to the pool's next available block.
//     *(uint8**)address = pool->NextBlock;

//     // Set the deallocated block as the next available block.
//     pool->NextBlock = address;

//     pool->Available++;

//     // If the pool is empty, free it.
//     if (pool->Available == pool->Capacity) virtPoolRelease(pool); 
// }
