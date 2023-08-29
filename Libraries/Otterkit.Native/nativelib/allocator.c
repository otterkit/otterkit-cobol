#include <stdio.h>
#include "allocator.h"

private VirtualHeap GlobalVirtualHeap;

void VirtualHeapInitialize(VirtualHeap* instance, uint64 size)
{
    // Avoid overwriting the allocator instance if it has already been initialized.
    if (instance->IsInitialized) return;

    // If the size is 0, then we allocate the default 2TiB of address space.
    if (size == 0) size = 2ull << 40;

    // Reserve the allocator's address space, and an additional 128KiB for alignment purposes.
    void* base = SystemAlloc(nullptr, size + 131072, SYS_PROTECTED, SYS_RESERVE);

    // Align the heap to a 128KiB boundary (needed to make deallocation work properly)
    uintptr aligned = ((uintptr)base + (131072 - 1)) & -131072;

    instance->BaseAddress = (uint8*)aligned;
    instance->Capacity = size;

    // Allocate the bit tree.
    instance->BitTree = (VirtualTree*)SystemAlloc(
        nullptr, sizeof(VirtualTree), SYS_READWRITE, SYS_ALLOCATE
    );
}

initializer void VirtualHeapInitializeDefault() { VirtualHeapInitialize(&GlobalVirtualHeap, 0); }

VirtualHeap* VirtualHeapGetGlobalInstance() { return &GlobalVirtualHeap; }

void VirtualPoolInitialize(VirtualPool* pool, uint64 blockLength)
{
    // Initialize the pool. Pool capacity is 128KiB / blockLength.
    pool->Capacity = 131072 / blockLength;
    pool->Available = pool->Capacity;
    pool->Initialized = 0;
    pool->BlockLength = blockLength;
    pool->NextBlock = 0;
}

VirtualPool* VirtualPoolCreate(VirtualHeap* allocator, uint64 blockLength)
{
    VirtualTree* tree = allocator->BitTree;

    uint64* root = &tree->Root;

    uint64 indexRoot = __tzcnt_u64(~(*root)); // 64

    uint64* trunk = &tree->Trunks[indexRoot];

    uint64 indexTrunk = __tzcnt_u64(~(*trunk)); // 4096

    uint64* branch = &tree->Branches[(indexRoot << 6) + indexTrunk];

    uint64 indexBranch = __tzcnt_u64(~(*branch)); // 131072

    uint64* twig = &tree->Twigs[(indexRoot << 12) + (indexTrunk << 6) + indexBranch];

    uint64 indexTwig = __tzcnt_u64(~(*twig)); // 4194304

    uint64* leaf = &tree->Leaves[(indexRoot << 18) + (indexTrunk << 12) + (indexBranch << 6) + indexTwig];

    uint64 indexLeaf = __tzcnt_u64(~(*leaf)); // 1073741824

    // Check for invalid pool allocation.
    // if (*leaf & (1ull << indexLeaf))
    // {
    //     printf("Invalid pool allocation!\n");
    //     return nullptr;
    // }

    // printf("Allocating pool at %llu %llu %llu %llu %llu\n", indexRoot, indexTrunk, indexBranch, indexTwig, indexLeaf);

    *leaf |= (1ull << indexLeaf);

    if (*leaf == ~0ull) *twig |= (1ull << indexTwig);

    if (*twig == ~0ull) *branch |= (1ull << indexBranch);

    if (*branch == ~0ull) *trunk |= (1ull << indexTrunk);

    if (*trunk == ~0ull) *root |= (1ull << indexRoot);

    // If the root is full, then the entire heap is full (all 128TiB of it)
    if (*root == ~0ull) return nullptr;

    VirtualPool* pool = ComputePoolAddress(
        allocator->BaseAddress, indexRoot, indexTrunk, indexBranch, indexTwig, indexLeaf
    );

    // Commit the pool's address space.
    pool = SystemCommit(pool, 131072);

    // Initialize the virtual pool.
    VirtualPoolInitialize(pool, blockLength);

    // Store the pool's bit tree index.
    pool->Index = PackVirtualIndex(
        indexRoot, indexTrunk, indexBranch, indexTwig, indexLeaf
    );

    return pool;
}

void VirtualPoolDecommit(VirtualHeap* allocator, VirtualPool* pool)
{
    VirtualIndex index = pool->Index;

    UnpackVirtualIndex(index);

    VirtualTree* tree = allocator->BitTree;

    uint64* root = &tree->Root;

    uint64* trunk = &tree->Trunks[indexRoot];

    uint64* branch = &tree->Branches[
        (indexRoot << 6) + indexTrunk
    ];

    uint64* twig = &tree->Twigs[
        (indexRoot << 12) + (indexTrunk << 6) + indexBranch
    ];

    uint64* leaf = &tree->Leaves[
        (indexRoot << 18) + (indexTrunk << 12) + (indexBranch << 6) + indexTwig
    ];

    // Check for invalid pool deallocation.
    // if (!(*leaf & (1ull << indexLeaf)))
    // {
    //     printf("Invalid pool deallocation!\n");
    //     return;
    // }

    // printf("Deallocating pool at %llu %llu %llu %llu %llu\n", indexRoot, indexTrunk, indexBranch, indexTwig, indexLeaf);

    *leaf &= ~(1ull << indexLeaf);

    if (*leaf == 0ull) *twig &= ~(1ull << indexTwig);

    if (*twig == 0ull) *branch &= ~(1ull << indexBranch);

    if (*branch == 0ull) *trunk &= ~(1ull << indexTrunk);

    if (*trunk == 0ull) *root &= ~(1ull << indexRoot);

    // Decommit the pool's address space.
    SystemDecommit(pool, 131072);
}

// Apparently, GCC can't optimize this function properly and generates a bunch of branches.
// This is a performance-critical function that is called every time a block is allocated.
// We can't afford to have it be slow due to branch mispredictions, please compile with Clang.
uint64 AlignLengthToSizeClass(uint64 length)
{
    // rdi = length (sysv calling convention)
    uint64 aligned;

    // test    rdi, rdi
    // mov     rcx, 1
    // cmovne  rcx, rdi
    if (length == 0) length = 1;

    // lea     rdx, [rcx + 127]
    // shr     rdx, 7
    // add     rdx, 56
    aligned = length + (128 - 1);
    aligned = (aligned >> 7) + 56;

    // lea     rax, [rcx + 15]
    // shr     rax, 4
    // cmp     rcx, 1025
    // cmovae  rax, rdx
    if (length <= 1024)
    {
        aligned = length + (16 - 1);
        aligned = aligned >> 4;
    }

    // dec     rax
    // ret
    return aligned - 1;
}

int main(void)
{
    return 0;
}

void* VirtualPoolAllocateSmall(VirtualPool* pool)
{
    uint8* block = nullptr;

    // If there are any uninitialized blocks, initialize one.
    if (pool->Initialized != pool->Capacity)
    {
        // Get the index of the uninitialized block.
        uint16 index = pool->Initialized++;
        // Get the address of the uninitialized block.
        uint8* block = (uint8*)pool + 16 + index * pool->BlockLength;
        // Set the next block index to the next uninitialized block.
        *(uint16*)block = index + 1;
    }

    // If there are any available blocks, allocate one.
    if (pool->Available != 0)
    {
        // Get the address of the next available block.
        block = (uint8*)pool + 16 + pool->NextBlock * pool->BlockLength;
        // Decrement the number of available blocks.
        pool->Available--;
        // If there are any available blocks left, set 
        // the next block index to the next available block.
        pool->NextBlock = pool->Available ? *(uint16*)block : 0;
    }

    return block;
}

void* VirtualHeapAllocateSmall(uint64 length)
{
    // Get the global allocator instance.
    VirtualHeap* allocator = VirtualHeapGetGlobalInstance();
    // Align the length to one of the small size classes.
    uint64 class = AlignLengthToSizeClass(length);
    // Fetch the allocator's immediate pool for the size class.
    VirtualPool* pool = allocator->Immediate[class];

    // Allocate a block from the pool if it's available.
    if (pool != nullptr)
    {
        // Allocate a block from the pool.
        void* block = VirtualPoolAllocateSmall(pool);
        // If the pool is now empty, set the immediate pool to null.
        if (pool->Available == 0)
        {
            allocator->Immediate[class] = nullptr;
        }

        return block;
    }

    // If an immediate pool is not available, allocate a new one.
    pool = VirtualPoolCreate(allocator, length);

    // If a new pool was successfully allocated
    if (pool != nullptr)
    {
        // Set it as the immediate pool for the size class.
        allocator->Immediate[class] = pool;
        // Allocate a block from the new pool.
        return VirtualPoolAllocateSmall(pool);
    }

    // Most likely, the heap is full.
    return nullptr;
}

void VirtualPoolDeallocateSmall(VirtualPool* pool, void* address)
{
    // Set the deallocated block's next block index to 
    // the next available block.
    *(uint16*)address = pool->NextBlock;
    // Get the deallocated block's absolute index.
    uint16 index = (uint8*)address - (uint8*)pool - 16;
    // Set the next available block to the deallocated 
    // block's relative index.
    pool->NextBlock = index / pool->BlockLength;
    // Increment the number of available blocks.
    pool->Available++;
}

void VirtualHeapDeallocateSmall(void* address)
{
    // Get the global allocator instance.
    VirtualHeap* allocator = VirtualHeapGetGlobalInstance();
    
    // Get the pool that the deallocated block belongs to.
    VirtualPool* pool = (VirtualPool*)((uintptr)address & -131072);

    // Deallocate the block from the pool.
    VirtualPoolDeallocateSmall(pool, address);

    uint64 class = AlignLengthToSizeClass(pool->BlockLength);

    // Get the immediate pool for the size class.
    VirtualPool** immediate = &allocator->Immediate[class];

    // Set the pool as the immediate pool for the size class, if there isn't one already.
    if (*immediate == nullptr) *immediate = pool;

    // Decommit the pool if it's empty and there's already an immediate pool for the size class.
    if (pool->Available == pool->Capacity && *immediate != nullptr)
    {
        VirtualPoolDecommit(allocator, pool);
    }
}

// void AllocatorTest(void)
// {
//     VirtualHeap* allocator = VirtualHeapGetGlobalInstance();

//     void* prev = nullptr;
//     uint64 count = 0;

//     for (uint64 i = 0; i < allocator->Capacity; i += (1ull << 17))
//     {
//         void* address = AllocatorCreateVirtualPool(allocator, 16);

//         // Test if we ran out of memory earlier than expected.
//         // This should never happen during this test (in practice, it will happen when the heap is full)
//         if (address == nullptr)
//         {
//             printf("Out of memory too early!\n");
//             break;
//         }

//         // Test if the address is not contiguous with the previous one.
//         // This error should never happen, otherwise the allocator indexing is broken.
//         if (prev != nullptr && (uintptr)address != (uintptr)prev + (1ull << 17))
//         {
//             printf("Address mismatch! %p %p %llu\n", prev, address, (uintptr)address - (uintptr)prev);
//             break;
//         }

//         // Test if the address is not aligned to a 128KiB boundary.
//         // This error should never happen, otherwise the allocator initializer is broken.
//         if ((uintptr)address & (131072 - 1))
//         {
//             printf("Address not aligned! %p\n", address);
//             break;
//         }

//         count++;
//         prev = address;
//     }

//     if ((uintptr)prev + 131072 > (uintptr)allocator->BaseAddress + allocator->Capacity)
//     {
//         printf("Address out of bounds! %p\n", prev);
//     }

//     printf("Allocated %llu blocks\n", count);

//     // Test if the allocator can release addresses properly.
//     for (uint64 i = 0; i < allocator->Capacity; i += (1ull << 17))
//     {
//         AllocatorDecommitVirtualPool(allocator, prev);
//         prev = (uint8*)prev - (1ull << 17);

//         count--;
//     }

//     if (count != 0) printf("Failed to release all addresses! %llu\n", count);
// }
