#include "CASPAL.h"
#include <stdio.h>

typedef struct vpool_t
{
    uint16 BlockLength;
    uint16 Initialized;
    uint16 Available;
    uint16 Capacity;
    uint8* NextBlock;
} VirtualPool;

typedef struct vsegment_t
{
    // Start of the segment address space.
    uint8* Memory;
    // Amount of available memory (in bytes).
    uint32 Available;
    // Amount of total memory (in bytes).
    uint32 Capacity;
    // Block length of all pools in this segment.
    uint16 BlockLength;

    // Immediate (directly available for allocation)
    // Cached (used when present, instead of creating a new pool)
    VirtualPool* Immediate;
    VirtualPool* Cached;

    // BitTree of available segment pools.
    // 32 bits (1 bit per 4 GiB)
    uint32 Root;
    // 1024 bits (1 bit per 128 MiB)
    uint32 Trunks[1 << 5];
    // 32768 bits (1 bit per 4 MiB)
    uint32 Branches[1 << 10];
    // 1048576 bits (1 bit per 128KiB)
    uint32 Leaves[1 << 15];
} VirtualSegment;

typedef struct vallocator_t
{
    // The virtual heap has 16 size classes, split into 4 categories.
    // Tiny: multiples of 16 bytes, from 16 to 128 bytes.
    // Small: multiples of 128 bytes, from 128 to 512 bytes.
    // Medium: start after 512 bytes, and double in size until 8KiB.
    // Large: start after 8KiB, and can be any arbitrary size (Large Object Pool).
    // [16, 32, 48, 64, 80, 96, 112, 128, 256, 384, 512, 1024, 2048, 4096, 8192, LOP]
    VirtualSegment* Segments[16];

    // Start of the heap's reserved address space.
    uint8* HeapAddress;

    uint8 IsInitialized;
} VirtualAllocator;

static VirtualAllocator GlobalAllocator;

void virtAllocatorInitialize(uint64 size)
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

    GlobalAllocator.HeapAddress = (uint8*)aligned;

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

initializer void virtAllocatorInitializeDefault() { virtAllocatorInitialize(0); }

void virtPoolCreate(VirtualSegment* segment)
{
    uint32* root = &segment->Root;

    uint32 rootIndex = __tzcnt_u32(~(*root)); // 32

    uint32* trunk = &segment->Trunks[rootIndex];

    uint32 trunkIndex = __tzcnt_u32(~(*trunk)); // 1024

    uint32* branch = &segment->Branches[
        (rootIndex << 5) + trunkIndex
    ];

    uint32 branchIndex = __tzcnt_u32(~(*branch)); // 32768

    uint32* leaf = &segment->Leaves[
        (rootIndex << 10) + (trunkIndex << 5) + branchIndex
    ];

    uint32 leafIndex = __tzcnt_u32(~(*leaf)); // 1048576

    *leaf |= (1u << leafIndex);

    if (*leaf == ~0u) *branch |= (1u << branchIndex);

    if (*branch == ~0u) *trunk |= (1u << trunkIndex);

    if (*trunk == ~0u) *root |= (1u << rootIndex);

    // If the root is full, then the entire segment is full (128GiB)
    // if (*root == ~0u) return nullptr;

    void* poolAddress = (
        segment->Memory
        + (rootIndex << 15)
        + (trunkIndex << 10)
        + (branchIndex << 5)
        + leafIndex
    );
    
    VirtualPool* pool = sysCommitMemory(poolAddress, 128 * 1024);

    // Pool is always 128KiB in size, header is 16 bytes.
    uint16 capacity = (128 * 1024 - 16) / segment->BlockLength;

    pool->BlockLength = segment->BlockLength;
    pool->Available = capacity;
    pool->Capacity = capacity;

    // Set the next block pointer to the first available memory address.
    // The first 16 bytes are reserved for the pool header.
    pool->NextBlock = (uint8*)pool + 16;

    // Set the pool in the segment's immediate pool pointer.
    segment->Immediate = pool;
}

void virtPoolRelease(VirtualSegment* segment, VirtualPool* pool)
{
    uint32* root = &segment->Root;

    uint32 rootIndex = (
        ((uintptr)pool - (uintptr)segment->Memory) >> 15
    );

    uint32* trunk = &segment->Trunks[
        rootIndex
    ];

    uint32 trunkIndex = (
        ((uintptr)pool - (uintptr)segment->Memory) >> 10
    ) & ((1 << 5) - 1);

    uint32* branch = &segment->Branches[
        (rootIndex << 5) + trunkIndex
    ];

    uint32 branchIndex = (
        ((uintptr)pool - (uintptr)segment->Memory) >> 5
    ) & ((1 << 10) - 1);

    uint32* leaf = &segment->Leaves[
        (rootIndex << 10) + (trunkIndex << 5) + branchIndex
    ];

    uint32 leafIndex = (
        ((uintptr)pool - (uintptr)segment->Memory)
    ) & ((1 << 15) - 1);

    *leaf &= ~(1u << leafIndex);

    if (*leaf == 0u) *branch &= ~(1u << branchIndex);

    if (*branch == 0u) *trunk &= ~(1u << trunkIndex);

    if (*trunk == 0u) *root &= ~(1u << rootIndex);

    // Decommit the pool's pages (128KiB), still keep it reserved.
    sysDecommitMemory(pool, 131072);
}

// uint64 AlignSizeClassC(uint64 size)
// {
//     if (size == 0) size++;

//     uint64 class = size;

//     if (size <= 8192)
//     {
//         class = 1ull << (64 - __builtin_clzll(size - 1));
//         class = ((class >> 11) + 12) - (class == 8192);
//     }
    
//     if (size <= 512)
//     {
//         class = (size + (128 - 1)) & -128;
//         class = (class >> 7) + 7;
//     }

//     if (size <= 128)
//     {
//         class = (size + (16 - 1)) & -16;
//         class = class >> 4;
//     }

//     return class - 1;
// }

// Apparently, GCC can't optimize this function properly, so we have to do it ourselves.
// Clang can optimize it just fine into branchless assembly, but GCC still generates branchy code.
uint64 AlignLengthToSegment(uint64 length) label ("AlignLengthToSegment");
#if defined AMD64
assembly (
    UsingIntelSyntax
    "AlignLengthToSegment:              \n"

    "# If length is 0, set it to 1      \n"
    "test    rdi, rdi                   \n"
    "mov     rax, 1                     \n"
    "cmove   rdi, rax                   \n"

    "# Align to the next power of 2     \n"
    "lea     rcx, [rdi - 1]             \n"
    "lzcnt   rcx, rcx                   \n"
    "mov     rdx, rcx                   \n"
    "neg     dl                         \n"
    "shlx    rax, rax, rdx              \n"

    "# Align into [512 .. 8192] classes \n"
    "shr     rax, 11                    \n"
    "xor     rdx, rdx                   \n"
    "cmp     rcx, 51                    \n"
    "sete    dl                         \n"
    "sub     rax, rdx                   \n"
    "add     rax, 12                    \n"
    
    "# Align into [128 .. 512] classes  \n"
    "lea     rcx, [rdi + 127]           \n"
    "shr     rcx, 7                     \n"
    "add     rcx, 7                     \n"
    "cmp     rdi, 513                   \n"
    "cmovae  rcx, rax                   \n"

    "# Align into [16 .. 128] classes   \n"
    "lea     rax, [rdi + 15]            \n"
    "shr     rax, 4                     \n"
    "cmp     rdi, 129                   \n"
    "cmovae  rax, rcx                   \n"

    "# Subtract 1 and return size class \n"
    "dec     rax                        \n"
    "ret                                \n"
);
#elif defined ARM64
assembly (
    "AlignSizeToSegment:            \n"
    "cmp     x0, #0                 \n"
    "mov     w8, #1                 \n"
    "csinc   x9, x0, xzr, ne        \n"
    "sub     x10, x9, #1            \n"
    "clz     x10, x10               \n"
    "neg     x11, x10               \n"
    "cmp     x10, #51               \n"
    "add     x10, x9, #127          \n"
    "lsr     x10, x10, #7           \n"
    "lsl     x8, x8, x11            \n"
    "mov     w11, #11               \n"
    "cinc    x11, x11, ne           \n"
    "add     x10, x10, #7           \n"
    "cmp     x9, #513               \n"
    "add     x8, x11, x8, lsr #11   \n"
    "add     x11, x9, #15           \n"
    "csel    x8, x10, x8, lo        \n"
    "lsr     x10, x11, #4           \n"
    "cmp     x9, #129               \n"
    "csel    x8, x10, x8, lo        \n"
    "sub     x0, x8, #1             \n"
    "ret                            \n"
);
#endif

int main()
{
    printf("Hello, world!\n");
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
