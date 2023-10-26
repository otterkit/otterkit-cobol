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

// Trying to keep the pool bookkeeping header's memory usage to a minimum.
StaticAssert(sizeof(VirtualPool) == 16, "VirtualPool size must be 16 bytes or less!")

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

// Virtual heap for dynamic memory allocation
typedef struct vheap_t
{
    // The base address of the heap.
    uint8* BaseAddress;
    // The total capacity of the heap, in bytes.
    uint64 Capacity;
    // The cached pool lookup table (indexed by size class).
    VirtualPool* Cached[312];
    // The bit tree of available virtual pools.
    VirtualTree* BitTree;
    // Whether the heap has been initialized.
    bool Initialized;
} VirtualHeap;   

// Virtual stack for static memory allocation
typedef struct vstack_t
{
    // The base address of the stack
    uint8* BaseAddress;
    // Current stack pointer position
    uint8* StackPointer;
    // The total capacity of the stack, in bytes.
    uint64 Capacity;
    // Whether the stack has been initialized.
    bool Initialized;
} VirtualStack;

#endif // VIRTUAL_ALLOCATOR_H
