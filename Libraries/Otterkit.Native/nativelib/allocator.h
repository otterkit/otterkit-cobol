#if !defined(VIRTUAL_ALLOCATOR_H)
#define VIRTUAL_ALLOCATOR_H

#include "CASPAL.h"

typedef struct vindex_t
{
    // 64 bits (1 bit per 2 TiB)
    uint8 Root; // 0 .. 63 stored.
    // 4096 bits (1 bit per 32 GiB)
    uint8 Trunk; // 0 .. 63 stored.
    // 131072 bits (1 bit per 1 GiB)
    uint8 Branch; // 0 .. 63 stored.
    // 4194304 bits (1 bit per 32 MiB)
    uint8 Twig; // 0 .. 63 stored.
    // 1073741824 bits (1 bit per 128 KiB)
    uint8 Leaf; // 0 .. 63 stored.
} VirtualIndex;

typedef struct vcache_t
{
    // Immediate pool, directly available for allocation.
    VirtualPool* Immediate;
    // Cached pool, available for allocation after initialization.
    VirtualPool* Cached;
} VirtualCache;

typedef struct vpool_t
{
    // The pool's total capacity, in blocks.
    uint16 Capacity;
    // The number of currently available blocks.
    uint16 Available;
    // The number of (lazily) initialized blocks.
    uint16 Initialized;
    // The size of each block, in bytes.
    uint16 BlockLength;
    // The offset of the next available block.
    uint16 NextBlock;
    // The bit tree index of the pool.
    VirtualIndex Index;
} VirtualPool;

StaticAssert(sizeof(VirtualPool) == 16, "VirtualPool size mismatch")

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

typedef struct vheap_t
{
    // The base address of the virtual heap.
    uint8* BaseAddress;
    // The total capacity of the virtual heap, in bytes.
    uint64 Capacity;
    // The immediate pool lookup table (indexed by size class).
    VirtualPool* Immediate[120];
    // The bit tree of available virtual pools.
    VirtualTree* BitTree;
    // Whether the heap has been initialized.
    uint8 IsInitialized;
} VirtualHeap;

#define ComputePoolAddress(base, root, trunk, branch, twig, leaf) \
    (void*)(                \
        (uint8*)base        \
        + ((root << 24)     \
        + (trunk << 18)     \
        + (branch << 12)    \
        + (twig << 6)       \
        + leaf) * 131072    \
    );

#define PackVirtualIndex(root, trunk, branch, twig, leaf) \
    ((VirtualIndex) {               \
        .Root = (uint8)root,        \
        .Trunk = (uint8)trunk,      \
        .Branch = (uint8)branch,    \
        .Twig = (uint8)twig,        \
        .Leaf = (uint8)leaf         \
    })             

#define UnpackVirtualIndex(index) \
    uint64 indexRoot = (uint64)index.Root;      \
    uint64 indexTrunk = (uint64)index.Trunk;    \
    uint64 indexBranch = (uint64)index.Branch;  \
    uint64 indexTwig = (uint64)index.Twig;      \
    uint64 indexLeaf = (uint64)index.Leaf;      

#endif // VIRTUAL_ALLOCATOR_H
