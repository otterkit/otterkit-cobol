#include "CASPAL.h"
#include <stdio.h>

typedef struct pool_t
{
    uint64 BlockLength;
    uint64 Initialized;
    uint64 Available;
    uint64 Capacity;
    uint8* NextBlock;
} MemoryPool;

typedef struct vheap_t
{
    // Start of the heap address space.
    void* AddressSpace;
    uint64 Available;
    uint64 Capacity;

    // The virtual heap has 16 size classes, split into 4 categories.
    // Tiny: multiples of 16 bytes, from 16 to 128 bytes.
    // Small: multiples of 128 bytes, from 128 to 512 bytes.
    // Medium: start after 512 bytes, and double in size until 8KiB.
    // Large: start after 8KiB, and can be any arbitrary size (Large Object Pool).
    // [16, 32, 48, 64, 80, 96, 112, 128, 256, 384, 512, 1024, 2048, 4096, 8192, LOP]
    MemoryPool* DynamicPools[16];

    // Bit tree of available memory.
    // 64 bits (1 bit per 32 GiB)
    uint64 Root;
    // 4096 bits (1 bit per 512 MiB)
    uint64 LevelOne[1 << 6];
    // 262144 bits (1 bit per 512 MiB)
    uint64 LevelTwo[1 << 12];
    // 16777216 bits (1 bit per 128KiB)
    uint64 Leaves[1 << 18];
} VirtualHeap;

static VirtualHeap* GlobalHeap = nullptr;

void ottrkHeapInitialize(uint64 size)
{
    // Avoid overwriting the heap if it has already been initialized.
    if (GlobalHeap) return;

    // If the size is 0, then we allocate the default 2TiB of address space.
    if (size == 0) size = 2ull << 40;

    // Reverse the address space, and an additional 8MiB for the heap header.
    void* address = sysReserveAddressSpace(size + (8 << 20));
    
    // Commit the first 8MiB of the address space.
    address = sysCommitMemory(address, 8 << 20);

    GlobalHeap = address;

    // Align the address space down to a 128KiB boundary, skipping the first 8MiB.
    uintptr aligned = (uintptr)(address + (8 << 20));
    aligned &= -128 * 1024;

    // Set the address space to the start of the reserved address space.
    GlobalHeap->AddressSpace = (void*)aligned;

    // Set the available and capacity to the amount of pools that can be created.
    GlobalHeap->Available = size >> 17;
    GlobalHeap->Capacity = size >> 17;
}

initializer void ottrkHeapInitializeDefault() { ottrkHeapInitialize(0); }

MemoryPool* ottrkPoolInitialize(uint16 blockLength)
{
    uint64* root = &GlobalHeap->Root;

    uint64 rootIndex = __tzcnt_u64(~(*root)); // 64

    uint64* levelOne = &GlobalHeap->LevelOne[
        rootIndex
    ];

    uint64 levelOneIndex = __tzcnt_u64(~(*levelOne)); // 4096

    uint64* levelTwo = &GlobalHeap->LevelTwo[
        (rootIndex << 6) + levelOneIndex
    ];

    uint64 levelTwoIndex = __tzcnt_u64(~(*levelTwo)); // 262144

    uint64* leaves = &GlobalHeap->Leaves[
        (rootIndex << 12) + (levelOneIndex << 6) + levelTwoIndex
    ];

    uint64 leavesIndex = __tzcnt_u64(~(*leaves)); // 1073741824

    *leaves |= (1ull << leavesIndex);

    if (*leaves == ~0ull) *levelTwo |= (1ull << levelTwoIndex);

    if (*levelTwo == ~0ull) *levelOne |= (1ull << levelOneIndex);

    if (*levelOne == ~0ull) *root |= (1ull << rootIndex);

    // If the root is full, then the entire address space is full (128TiB)
    if (*root == ~0ull) return nullptr;

    void* poolAddress = (
        GlobalHeap->AddressSpace
        + (rootIndex << 18) 
        + (levelOneIndex << 12) 
        + (levelTwoIndex << 6) 
        + leavesIndex
    );
    
    MemoryPool* pool = sysCommitMemory(poolAddress, 128 * 1024);

    // Pool is always 128KiB in size, we can easily calculate the capacity.
    uint16 capacity = (128 * 1024 - 32) / blockLength;

    pool->BlockLength = blockLength;
    pool->Available = capacity;
    pool->Capacity = capacity;

    // Set the next block pointer to the first available memory address.
    // The first 32 bytes are reserved for the pool header.
    pool->NextBlock = (uint8*)pool + 32;

    // Set the pool in the global heap.
    GlobalHeap->DynamicPools[blockLength >> 4] = pool;

    return pool;
}

void ottrkPoolRelease(MemoryPool* pool)
{
    uint64* root = &GlobalHeap->Root;

    uint64 rootIndex = (
        ((uintptr)pool - (uintptr)GlobalHeap->AddressSpace) >> 18
    );

    uint64* levelOne = &GlobalHeap->LevelOne[
        rootIndex
    ];

    uint64 levelOneIndex = (
        ((uintptr)pool - (uintptr)GlobalHeap->AddressSpace) >> 12
    ) & ((1 << 6) - 1);

    uint64* levelTwo = &GlobalHeap->LevelTwo[
        (rootIndex << 6) + levelOneIndex
    ];

    uint64 levelTwoIndex = (
        ((uintptr)pool - (uintptr)GlobalHeap->AddressSpace) >> 6
    ) & ((1 << 12) - 1);

    uint64* leaves = &GlobalHeap->Leaves[
        (rootIndex << 12) + (levelOneIndex << 6) + levelTwoIndex
    ];

    uint64 leavesIndex = (
        ((uintptr)pool - (uintptr)GlobalHeap->AddressSpace)
    ) & ((1 << 18) - 1);

    *leaves &= ~(1ull << leavesIndex);

    if (*leaves == 0ull) *levelTwo &= ~(1ull << levelTwoIndex);

    if (*levelTwo == 0ull) *levelOne &= ~(1ull << levelOneIndex);

    if (*levelOne == 0ull) *root &= ~(1ull << rootIndex);

    if (GlobalHeap->DynamicPools[pool->BlockLength >> 4] == pool)
    {
        GlobalHeap->DynamicPools[pool->BlockLength >> 4] = nullptr;
    }

    // Decommit the pool.
    sysDecommitMemory(pool, 128 * 1024);
}

void* ottrkPoolAlloc(MemoryPool* pool)
{
    // If there are any uninitialized blocks, initialize one.
    if (pool->Initialized < pool->Capacity)
    {
        // Get the address of the next uninitialized block.
        uint8* uninitialized = (
            (uint8*)pool + 32 + pool->Initialized * pool->BlockLength
        );
        
        // Set the next block pointer to the next available block.
        *(uint8**)uninitialized = uninitialized + pool->BlockLength;

        pool->Initialized++;
    }

    uint8* block = nullptr;

    // If there are any available blocks, allocate one.
    if (pool->Available)
    {
        block = pool->NextBlock;
        pool->Available--;

        // If there are any available blocks left, set the next block pointer to the next available block.
        pool->NextBlock = (pool->Available) ? *(uint8**)block : nullptr;
    }

    return block;
}

void ottrkPoolDealloc(MemoryPool* pool, void* address)
{
    // Set the next block pointer to the pool's next available block.
    *(uint8**)address = pool->NextBlock;

    // Set the deallocated block as the next available block.
    pool->NextBlock = address;

    pool->Available++;

    // If the pool is empty, free it.
    if (pool->Available == pool->Capacity) ottrkPoolRelease(pool); 
}

void* ottrkSmallAlloc(uint64 size)
{
    // Align the size up to the nearest 16 bytes.
    size = (size + (16 - 1)) & -16;

    // Get the pool index from the size.
    uint16 poolIndex = (size / 16) - 1;

    // Get the pool from the global heap.
    MemoryPool* pool = GlobalHeap->DynamicPools[poolIndex];

    // If the pool is not initialized, initialize it.
    if (!pool || !pool->Available) pool = ottrkPoolInitialize(size);

    // Allocate a block from the pool.
    return ottrkPoolAlloc(pool);
}

void ottrkSmallDealloc(void* address)
{
    // Get the pool from the address.
    MemoryPool* pool = (MemoryPool*)(
        (uintptr)address & -(128 << 10)
    );

    // Deallocate the block from the pool.
    ottrkPoolDealloc(pool, address);
}
