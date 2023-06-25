
#ifndef U8_UNICODE_H
#define U8_UNICODE_H

#include <stdint.h>

typedef struct
{
    uint32_t CodePoint;
    uint32_t UpperCase;
    uint32_t CaseFolded;
} UnicodeTableEntry;

#endif
