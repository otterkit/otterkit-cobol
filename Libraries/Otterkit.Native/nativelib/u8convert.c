// Adapted from: https://github.com/simdutf/simdutf/
// License: MIT or Apache-2.0 (See NOTICE in root directory)

#include "u8simd.h"
#include "common.h"

public int32_t u8CharFromCodepoint(uint32_t codepoint, uint8_t* destination) 
{
    int32_t length = 0;

    if ((codepoint & 0xFFFFFF80) == 0) 
    {
        // will generate one UTF-8 bytes
        destination[0] = (uint8_t)codepoint;

        length = 1;
    } 
    else if ((codepoint & 0xFFFFF800) == 0) 
    {
        // will generate two UTF-8 bytes
        // we have 0b110XXXXX 0b10XXXXXX
        destination[0] = (uint8_t)((codepoint >> 6) | 0b11000000);
        destination[1] = (uint8_t)((codepoint & 0b111111) | 0b10000000);

        length = 2;
    } 
    else if ((codepoint & 0xFFFF0000) == 0) 
    {
        // will generate three UTF-8 bytes
        // we have 0b1110XXXX 0b10XXXXXX 0b10XXXXXX
        destination[0] = (uint8_t)((codepoint >> 12) | 0b11100000);
        destination[1] = (uint8_t)(((codepoint >> 6) & 0b111111) | 0b10000000);
        destination[2] = (uint8_t)((codepoint & 0b111111) | 0b10000000);

        length = 3;
    } 
    else
    {
        // will generate four UTF-8 bytes
        // we have 0b11110XXX 0b10XXXXXX 0b10XXXXXX 0b10XXXXXX
        destination[0] = (uint8_t)((codepoint >> 18) | 0b11110000);
        destination[1] = (uint8_t)(((codepoint >> 12) & 0b111111) | 0b10000000);
        destination[2] = (uint8_t)(((codepoint >> 6) & 0b111111) | 0b10000000);
        destination[3] = (uint8_t)((codepoint & 0b111111) | 0b10000000);

        length = 4;
    }

    return length;
}

public int32_t u8CharToCodepoint(const uint8_t* source, uint32_t* destination) 
{
    uint8_t leadingByte = source[0];
    int32_t length = 0;

    if (leadingByte < 0b10000000)
    {
        // We have a one-byte UTF-8 codepoint (ASCII)
        *destination = (uint32_t)(leadingByte);

        length = 1;
    } 
    else if ((leadingByte & 0b11100000) == 0b11000000) 
    {
        // We have a two-byte UTF-8 codepoint
        *destination = (uint32_t)(
            ((leadingByte & 0b00011111) << 6) 
            | (source[1] & 0b00111111)
        );

        length = 2;
    } 
    else if ((leadingByte & 0b11110000) == 0b11100000) 
    {
        // We have a three-byte UTF-8 codepoint
        uint32_t codepoint = (
            ((leadingByte &0b00001111) << 12) 
            | ((source[1] &0b00111111) << 6) 
            | (source[2] &0b00111111)
        );

        *destination = (uint32_t)codepoint;

        length = 3;
    } 
    else if ((leadingByte & 0b11111000) == 0b11110000) 
    { 
        // we have a 4-byte UTF-8 codepoint.
        uint32_t codepoint = (
            ((leadingByte & 0b00000111) << 18)
            | ((source[1] &0b00111111) << 12)
            | ((source[2] &0b00111111) << 6) 
            | (source[3] &0b00111111)
        );

        *destination = (uint32_t)codepoint;

        length = 4;
    }
    else
    {
        // return 0 indicating that we haven't converted anything.
        return 0;
    }

    return length;
}