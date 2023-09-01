#include "allocator.h" // includes CASPAL.H

private VirtualHeap GlobalHeap;

VirtualHeap* VirtualHeapGetGlobalHeap(void) { 
    return &GlobalHeap; 
}

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

initializer void VirtualHeapDefault(void) { 
    VirtualHeapInitialize(&GlobalHeap, 0); 
}

private inline VirtualPool* VirtualPoolCreate(VirtualHeap* allocator, uint64 blockLength) {
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

    *leaf |= (1ull << indexLeaf);

    if (*leaf == ~0ull) *twig |= (1ull << indexTwig);

    if (*twig == ~0ull) *branch |= (1ull << indexBranch);

    if (*branch == ~0ull) *trunk |= (1ull << indexTrunk);

    if (*trunk == ~0ull) *root |= (1ull << indexRoot);

    // If the root is full, then the entire heap is full (all 128TiB of it)
    if (*root == ~0ull) return nullptr;

    VirtualPool* pool = (void*)(
        allocator->BaseAddress 
        + ((indexRoot << 24) 
        + (indexTrunk << 18) 
        + (indexBranch << 12) 
        + (indexTwig << 6) 
        + indexLeaf) * 131072 
    );

    // Commit the pool's address space.
    pool = SystemCommit(pool, 131072);

    // Initialize the pool. Size is always 128KiB.
    pool->Capacity = 131072 / blockLength;
    pool->Available = pool->Capacity;
    pool->BlockLength = blockLength;

    // Pack the index for later use during deallocation
    VirtualIndex index = {
        .Root   = (uint8)indexRoot, 
        .Trunk  = (uint8)indexTrunk, 
        .Branch = (uint8)indexBranch, 
        .Twig   = (uint8)indexTwig, 
        .Leaf   = (uint8)indexLeaf 
    };

    pool->Index = index;

    return pool;
}

private inline void VirtualPoolDecommit(VirtualHeap* allocator, VirtualPool* pool) {
    VirtualIndex index = pool->Index;

    // Unpack the bit index back to uint64s.
    uint64 indexRoot   = index.Root;
    uint64 indexTrunk  = index.Trunk;
    uint64 indexBranch = index.Branch;
    uint64 indexTwig   = index.Twig;
    uint64 indexLeaf   = index.Leaf;

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
private inline uint64 AlignLengthToSizeClass(uint64 length) {
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
    if (length <= 1024) {
        aligned = length + (16 - 1);
        aligned = aligned >> 4;
    }

    // dec     rax
    // ret
    return aligned - 1;
}

void* DynamicAlloc(uint64 length) {
    // Get the global heap instance.
    VirtualHeap* heap = &GlobalHeap;
    // Align the length to one of the small size classes.
    uint64 class = AlignLengthToSizeClass(length);
    // Fetch the heap's cached pool for the size class.
    VirtualPool* pool = heap->Cached[class];

    // Go to the fast path, if a cached pool is available.
    if likely(true, pool != nullptr) goto AllocFastPath;

    // If it's not available, allocate a new one.
    pool = VirtualPoolCreate(heap, length);

    // If we failed to allocate a new pool, most likely the heap is full.
    if likely(false, pool == nullptr) return nullptr;
    
    // If we successfully allocated a new pool:
    // Set it as the cached pool for the size class.
    heap->Cached[class] = pool;

    AllocFastPath:/* empty */;
    // Allocate a block from the pool.
    uint8* block = nullptr;

    // If there are any uninitialized blocks, initialize one.
    if (pool->Initialized != pool->Capacity) {
        // Get the index of the uninitialized block.
        uint16 index = pool->Initialized++;
        // Get the address of the uninitialized block.
        block = (uint8*)pool + 16 + index * pool->BlockLength;
        // Set the next block index to the next uninitialized block.
        *(uint16*)block = index + 1;
    }

    // Get the address of the next available block.
    block = (uint8*)pool + 16 + pool->NextBlock * pool->BlockLength;
    // Decrement the number of available blocks.
    pool->Available--;
    // If there are any available blocks left, set 
    // the next block index to the next available block.
    pool->NextBlock = pool->Available ? *(uint16*)block : 0;
    
    // If the pool is now empty, set the cached pool to null.
    if likely(false, pool->Available == 0) {
        heap->Cached[class] = nullptr;
    }

    return block;
}

void DynamicDealloc(void* address) {
    // Get the global heap instance.
    VirtualHeap* heap = &GlobalHeap;
    // Get the pool that the deallocated block belongs to.
    VirtualPool* pool = (VirtualPool*)((uintptr)address & -131072);

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

    uint64 class = AlignLengthToSizeClass(pool->BlockLength);

    // Get the cached pool for the size class.
    VirtualPool** cached = &heap->Cached[class];
    // Set the pool as the cached pool for the size class, if there isn't one already.
    if (*cached == nullptr) *cached = pool;

    // Decommit the pool if it's empty and there's already a cached pool for the size class.
    if (*cached != pool && pool->Available == pool->Capacity) {
        VirtualPoolDecommit(heap, pool);
    }
}


