// Adapted from: https://github.com/simdutf/simdutf/
// License: MIT or Apache-2.0 (See NOTICE in root directory)

#include "common.h"

public int32 u8CharFromCodepoint(uint32 codepoint, uint8* destination) 
{
    int32 length = 0;

    if ((codepoint & 0xFFFFFF80) == 0) 
    {
        // will generate one UTF-8 bytes
        destination[0] = (uint8)codepoint;

        length = 1;
    } 
    else if ((codepoint & 0xFFFFF800) == 0) 
    {
        // will generate two UTF-8 bytes
        // we have 0b110XXXXX 0b10XXXXXX
        destination[0] = (uint8)((codepoint >> 6) | 0b11000000);
        destination[1] = (uint8)((codepoint & 0b111111) | 0b10000000);

        length = 2;
    } 
    else if ((codepoint & 0xFFFF0000) == 0) 
    {
        // will generate three UTF-8 bytes
        // we have 0b1110XXXX 0b10XXXXXX 0b10XXXXXX
        destination[0] = (uint8)((codepoint >> 12) | 0b11100000);
        destination[1] = (uint8)(((codepoint >> 6) & 0b111111) | 0b10000000);
        destination[2] = (uint8)((codepoint & 0b111111) | 0b10000000);

        length = 3;
    } 
    else
    {
        // will generate four UTF-8 bytes
        // we have 0b11110XXX 0b10XXXXXX 0b10XXXXXX 0b10XXXXXX
        destination[0] = (uint8)((codepoint >> 18) | 0b11110000);
        destination[1] = (uint8)(((codepoint >> 12) & 0b111111) | 0b10000000);
        destination[2] = (uint8)(((codepoint >> 6) & 0b111111) | 0b10000000);
        destination[3] = (uint8)((codepoint & 0b111111) | 0b10000000);

        length = 4;
    }

    return length;
}

public int32 u8CharToCodepoint(const uint8* source, uint32* destination) 
{
    uint8 leadingByte = source[0];
    int32 length = 0;

    if (leadingByte < 0b10000000)
    {
        // We have a one-byte UTF-8 codepoint (ASCII)
        *destination = (uint32)(leadingByte);

        length = 1;
    } 
    else if ((leadingByte & 0b11100000) == 0b11000000) 
    {
        // We have a two-byte UTF-8 codepoint
        *destination = (uint32)(
            ((leadingByte & 0b00011111) << 6) 
            | (source[1] & 0b00111111)
        );

        length = 2;
    } 
    else if ((leadingByte & 0b11110000) == 0b11100000) 
    {
        // We have a three-byte UTF-8 codepoint
        uint32 codepoint = (
            ((leadingByte &0b00001111) << 12) 
            | ((source[1] &0b00111111) << 6) 
            | (source[2] &0b00111111)
        );

        *destination = (uint32)codepoint;

        length = 3;
    } 
    else if ((leadingByte & 0b11111000) == 0b11110000) 
    { 
        // we have a 4-byte UTF-8 codepoint.
        uint32 codepoint = (
            ((leadingByte & 0b00000111) << 18)
            | ((source[1] &0b00111111) << 12)
            | ((source[2] &0b00111111) << 6) 
            | (source[3] &0b00111111)
        );

        *destination = (uint32)codepoint;

        length = 4;
    }
    else
    {
        // return 0 indicating that we haven't converted anything.
        return 0;
    }

    return length;
}