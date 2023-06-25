#include "common.h"
#include "u8unicode.h"

extern const UnicodeTableEntry u32BMP[];

_export void u8CaseFoldCodepoint(uint32_t *codepoint, uint32_t *casefolded)
{
    const UnicodeTableEntry *lookup = &u32BMP[*codepoint];

    *casefolded = lookup->CaseFolded;
}

_export int32_t u8CaseFoldEquals(uint32_t *left, uint32_t *right)
{
    uint32_t leftFolded, rightFolded;

    u8CaseFoldCodepoint(left, &leftFolded);
    u8CaseFoldCodepoint(right, &rightFolded);

    return leftFolded == rightFolded;
}
