#include <stdint.h>
#include <x86intrin.h>
#include <string.h>

#include "common.h"

typedef union u8TrieData
{
    uint64_t bitmap;
    uint8_t bytes[8];
} u8tdata_t;

typedef struct u8TrieNode
{
    uint32_t offset;
    u8tdata_t data;
} u8tnode_t;

// Set a bit in a 64-bit bitmap, using the byte as the index.
#define I(byte) | 1UL << (byte & 0b00111111)

// Static compressed trie for UTF-8 casefolding, more compact than a hash table
// and faster than a binary search tree.
const u8tnode_t u8UpperTrie[] = 
{
    { /* root */ .offset = 1, .data = { .bitmap = 0UL I(0xF0) } },
    { /* 0xF0 */ .offset = 2, .data = { .bitmap = 0UL I(0x9E) } },
    { /* 0x9E */ .offset = 3, .data = { .bitmap = 0UL I(0xA4) I(0xA5) } },
    { /* 0xA4 */ .offset = 5, .data = { .bitmap = 0UL I(0xA2) I(0xA3) I(0xA4) I(0xA5) I(0xA6) I(0xA7) I(0xA8) I(0xA9) I(0xAA) I(0xAB) I(0xAC) I(0xAD) I(0xAE) I(0xAF) I(0xB0) I(0xB1) I(0xB2) I(0xB3) I(0xB4) I(0xB5) I(0xB6) I(0xB7) I(0xB8) I(0xB9) I(0xBA) I(0xBB) I(0xBC) I(0xBD) I(0xBE) I(0xBF) } },
    { /* 0xA5 */ .offset = 35, .data = { .bitmap = 0UL I(0x80) I(0x81) I(0x82) I(0x83) } },
    { /* 0xA2 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x80, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xA3 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x81, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xA4 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x82, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xA5 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x83, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xA6 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x84, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xA7 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x85, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xA8 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x86, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xA9 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x87, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xAA */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x88, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xAB */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x89, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xAC */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x8A, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xAD */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x8B, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xAE */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x8C, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xAF */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x8D, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB0 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x8E, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB1 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x8F, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB2 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x90, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB3 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x91, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB4 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x92, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB5 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x93, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB6 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x94, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB7 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x95, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB8 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x96, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xB9 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x97, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xBA */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x98, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xBB */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x99, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xBC */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x9A, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xBD */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x9B, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xBE */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x9C, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0xBF */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x9D, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0x80 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x9E, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0x81 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0x9F, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0x82 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0xA0, 0x00, 0x00, 0x00, 0x04 } } },
    { /* 0x83 */ .offset = 0, .data = { .bytes = { 0xF0, 0x9E, 0xA4, 0xA1, 0x00, 0x00, 0x00, 0x04 } } },
};

// Memory and thread safety notes:
// The trie is static and read-only, so it should be safe to read from multiple threads.
// The algorithm assumes that the data is null-terminated, and only contains a single valid UTF-8 codepoint.
// It assumes that the destination buffer is large enough to hold the result (at least 6 bytes).
_export int32_t u8TrieSearch(const uint8_t *data, uint8_t *destination)
{
    // The trie is a prefix tree, ours is stored as a static array to avoid dynamic allocation.
    // Each node has a bitmap of the possible next bytes and the offset of its first child node.

    // Fetch the root node.
    const u8tnode_t *root = u8UpperTrie;
        
    const u8tnode_t *node = root;

    // Walk the trie until we reach a leaf node or run out of data.
    // Data needs to be null-terminated, loop will break when it reaches it.
    while (*data)
    {
        // The trie is designed to be used with non-ASCII UTF-8 data, so we only need to look at the last 6 bits.
        // The first two bits are always either 10xx.. or 11xx.., so we can toggle them off.
        uint8_t byte = *data++ & 0b00111111;

        // The above operation will leave us with a compressed byte with maximum value of 64.
        // This is a huge space optimization, since we can use a single uint64_t to store the bitmap.
        // The bitmap is a 64-bit integer with each bit representing a possible next byte.
        uint64_t bitmap = node->data.bitmap;

        // If the byte is not in the current node's bitmap, we can stop searching.
        // The byte sequence is not in the trie.
        // We return 0 to indicate that the search failed and no data was copied.
        if (!((bitmap >> byte) & 1UL)) return 0;

        // Count the number of bits set before the current byte.
        // This plus the offset of the current node's first child node is the index of the next node.
        uint64_t count = _mm_popcnt_u64(bitmap & ((1UL << byte) - 1));

        // Fetch the next node.
        node = &root[node->offset + count];

        // If the node has a 0 offset, it's a leaf node.
        // We can stop searching, we found the byte sequence in the trie.
        if (!node->offset) break;
    }

    // Copy the data from the leaf node to the destination buffer.
    memcpy(destination, node->data.bytes, 6);

    // Return the length of the copied data.
    return (int32_t)node->data.bytes[7];
}
