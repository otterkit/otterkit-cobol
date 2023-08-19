// Adapted from: https://github.com/cyb70289/utf8
// License: MIT (See NOTICE in root directory)

#include "CASPAL.h"

//
// http://www.unicode.org/versions/Unicode15.0.0/ch03.pdf - page 125
//
// Table 3-7. Well-Formed UTF-8 Byte Sequences
//
// +--------------------+------------+-------------+------------+-------------+
// | Code Points        | First Byte | Second Byte | Third Byte | Fourth Byte |
// +--------------------+------------+-------------+------------+-------------+
// | U+0000..U+007F     | 00..7F     |             |            |             |
// +--------------------+------------+-------------+------------+-------------+
// | U+0080..U+07FF     | C2..DF     | 80..BF      |            |             |
// +--------------------+------------+-------------+------------+-------------+
// | U+0800..U+0FFF     | E0         | A0..BF      | 80..BF     |             |
// +--------------------+------------+-------------+------------+-------------+
// | U+1000..U+CFFF     | E1..EC     | 80..BF      | 80..BF     |             |
// +--------------------+------------+-------------+------------+-------------+
// | U+D000..U+D7FF     | ED         | 80..9F      | 80..BF     |             |
// +--------------------+------------+-------------+------------+-------------+
// | U+E000..U+FFFF     | EE..EF     | 80..BF      | 80..BF     |             |
// +--------------------+------------+-------------+------------+-------------+
// | U+10000..U+3FFFF   | F0         | 90..BF      | 80..BF     | 80..BF      |
// +--------------------+------------+-------------+------------+-------------+
// | U+40000..U+FFFFF   | F1..F3     | 80..BF      | 80..BF     | 80..BF      |
// +--------------------+------------+-------------+------------+-------------+
// | U+100000..U+10FFFF | F4         | 80..8F      | 80..BF     | 80..BF      |
// +--------------------+------------+-------------+------------+-------------+
//

#define IsAscii(first) ((first) <= 0x7F)

private inline int32 IsValidTwoByteSequence(int32 length, uint8 first, uint8 second)
{
    return length >= 2 && first >= 0xC2 && first <= 0xDF && (int8)second <= (int8)0xBF;
}

private inline int32 IsValidThreeByteSequence(int32 secondValid, int32 thirdValid, uint8 first, uint8 second)
{
    return (secondValid && thirdValid)
        && ((first == 0xE0 && second >= 0xA0)
        || (first >= 0xE1 && first <= 0xEC)
        || (first == 0xED && second <= 0x9F)
        || (first >= 0xEE && first <= 0xEF));
}

private inline int32 IsValidFourByteSequence(int32 secondValid, int32 thirdValid, int32 fourthValid, uint8 first, uint8 second)
{
    return (secondValid && thirdValid && fourthValid)
        && ((first == 0xF0 && second >= 0x90)
        || (first >= 0xF1 && first <= 0xF3)
        || (first == 0xF4 && second <= 0x8F));
}

// Return 0 on success or 1-based index of first error char
public int32 u8ScalarAlgorithm(const uint8 *source, int32 len)
{
    int32 err_pos = 1;

    while (len) 
    {
        const uint8 first = source[0];
        int32 bytes;

        // 00..7F
        if (IsAscii(first)) 
        {
            bytes = 1;
        }
        // C2..DF, 80..BF
        else if (IsValidTwoByteSequence(len, first, source[1])) 
        {
            bytes = 2;
        } 
        else if (len >= 3) 
        {
            const uint8 second = source[1];

            // Is second, third between 0x80 ~ 0xBF
            const int secondValid = (int8)second <= (int8)0xBF;
            const int thirdValid = (int8)source[2] <= (int8)0xBF;

            // E0, A0..BF, 80..BF 
            // E1..EC, 80..BF, 80..BF 
            // ED, 80..9F, 80..BF 
            // EE..EF, 80..BF, 80..BF
            if (IsValidThreeByteSequence(secondValid, thirdValid, first, second))
            {
                bytes = 3;
            } 
            else if (len >= 4)
            {
                // Is fourth between 0x80 ~ 0xBF
                const int fourthValid = (int8)source[3] <= (int8)0xBF;

                // F0, 90..BF, 80..BF, 80..BF
                // F1..F3, 80..BF, 80..BF, 80..BF
                // F4, 80..8F, 80..BF, 80..BF
                if (IsValidFourByteSequence(secondValid, thirdValid, fourthValid, first, second)) 
                {
                    bytes = 4;
                } 
                else 
                {
                    return err_pos;
                }
            } 
            else 
            {
                return err_pos;
            }
        } 
        else 
        {
            return err_pos;
        }

        len -= bytes;
        err_pos += bytes;
        source += bytes;
    }

    return 0;
}

#if defined(AMD64)
// Return 0 on success or -1 on error
public int u8RangeAlgorithm(const uint8 *source, int32 length)
{
    if (length >= 16) 
    {
        vec128i prevInput = _mm_setzero_si128();
        vec128i prevFirstLength = _mm_setzero_si128();

        // 
        // Map high nibble of "First Byte" to legal character length minus 1
        // 0x00 ~ 0xBF --> 0
        // 0xC0 ~ 0xDF --> 1
        // 0xE0 ~ 0xEF --> 2
        // 0xF0 ~ 0xFF --> 3
        // 
        const vec128i firstLengthTable = _mm_setr_epi8(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3
        );

        // Map "First Byte" to 8-th item of range table (0xC2 ~ 0xF4)
        const vec128i firstRangeTable = _mm_setr_epi8(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8
        );

        // 
        // Range table, map range index to min and max values
        // Index 0    : 00 ~ 7F (First Byte, ascii)
        // Index 1,2,3: 80 ~ BF (Second, Third, Fourth Byte)
        // Index 4    : A0 ~ BF (Second Byte after E0)
        // Index 5    : 80 ~ 9F (Second Byte after ED)
        // Index 6    : 90 ~ BF (Second Byte after F0)
        // Index 7    : 80 ~ 8F (Second Byte after F4)
        // Index 8    : C2 ~ F4 (First Byte, non ascii)
        // Index 9~15 : illegal: i >= 127 && i <= -128
        // 
        const vec128i rangeMinTable = _mm_setr_epi8(
            0x00, 0x80, 0x80, 0x80, 0xA0, 0x80, 0x90, 0x80,
            0xC2, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F
        );

        const vec128i rangeMaxTable = _mm_setr_epi8(
            0x7F, 0xBF, 0xBF, 0xBF, 0xBF, 0x9F, 0xBF, 0x8F,
            0xF4, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80
        );

        //
        // Tables for fast handling of four special First Bytes (E0,ED,F0,F4), after
        // which the Second Byte are not 80~BF. It contains "range index adjustment".
        // +------------+---------------+------------------+----------------+
        // | First Byte | original range| range adjustment | adjusted range |
        // +------------+---------------+------------------+----------------+
        // | E0         | 2             | 2                | 4              |
        // +------------+---------------+------------------+----------------+
        // | ED         | 2             | 3                | 5              |
        // +------------+---------------+------------------+----------------+
        // | F0         | 3             | 3                | 6              |
        // +------------+---------------+------------------+----------------+
        // | F4         | 4             | 4                | 8              |
        // +------------+---------------+------------------+----------------+
        //

        // index1 -> E0, index14 -> ED
        const vec128i xDfxEeTable = _mm_setr_epi8(
            0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0
        );

        // index1 -> F0, index5 -> F4
        const vec128i xEfxFeTable = _mm_setr_epi8(
            0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        );

        vec128i error = _mm_setzero_si128();

        while (length >= 16) 
        {
            const vec128i input = _mm_loadu_si128((const vec128i *)source);

            // highNibbles = input >> 4
            const vec128i highNibbles = _mm_and_si128(
                _mm_srli_epi16(input, 4), _mm_set1_epi8(0x0F)
            );

            // firstLength = legal character length minus 1
            // 0 for 00~7F, 1 for C0~DF, 2 for E0~EF, 3 for F0~FF
            // firstLength = firstLengthTable[highNibbles]
            vec128i firstLength = _mm_shuffle_epi8(firstLengthTable, highNibbles);

            // First Byte: set range index to 8 for bytes within 0xC0 ~ 0xFF
            // range = firstRangeTable[highNibbles]
            vec128i range = _mm_shuffle_epi8(firstRangeTable, highNibbles);

            // Second Byte: set range index to firstLength
            // 0 for 00~7F, 1 for C0~DF, 2 for E0~EF, 3 for F0~FF
            // range |= (firstLength, prevFirstLength) << 1 byte
            range = _mm_or_si128(
                range, _mm_alignr_epi8(firstLength, prevFirstLength, 15)
            );

            // Third Byte: set range index to saturate_sub(firstLength, 1)
            // 0 for 00~7F, 0 for C0~DF, 1 for E0~EF, 2 for F0~FF
            // tmp = (firstLength, prevFirstLength) << 2 bytes
            vec128i tmp = _mm_alignr_epi8(firstLength, prevFirstLength, 14);

            // tmp = saturate_sub(tmp, 1)
            tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(1));

            // range |= tmp
            range = _mm_or_si128(range, tmp);

            // Fourth Byte: set range index to saturate_sub(firstLength, 2)
            // 0 for 00~7F, 0 for C0~DF, 0 for E0~EF, 1 for F0~FF
            // tmp = (firstLength, prevFirstLength) << 3 bytes
            tmp = _mm_alignr_epi8(firstLength, prevFirstLength, 13);

            // tmp = saturate_sub(tmp, 2)
            tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(2));

            // range |= tmp
            range = _mm_or_si128(range, tmp);

            //
            // Now we have below range indices caluclated
            // Correct cases:
            // - 8 for C0~FF
            // - 3 for 1st byte after F0~FF
            // - 2 for 1st byte after E0~EF or 2nd byte after F0~FF
            // - 1 for 1st byte after C0~DF or 2nd byte after E0~EF or
            //         3rd byte after F0~FF
            // - 0 for others
            // Error cases:
            //   9,10,11 if non ascii First Byte overlaps
            //   E.g., F1 80 C2 90 --> 8 3 10 2, where 10 indicates error
            //

            // Adjust Second Byte range for special First Bytes(E0,ED,F0,F4)
            // Overlaps lead to index 9~15, which are illegal in range table
            vec128i shift1, pos, range2;

            // shift1 = (input, prevInput) << 1 byte 
            shift1 = _mm_alignr_epi8(input, prevInput, 15);
            pos = _mm_sub_epi8(shift1, _mm_set1_epi8(0xEF));

            //
            // shift1:  | EF  F0 ... FE | FF  00  ... ...  DE | DF  E0 ... EE |
            // pos:     | 0   1      15 | 16  17           239| 240 241    255|
            // pos-240: | 0   0      0  | 0   0            0  | 0   1      15 |
            // pos+112: | 112 113    127|       >= 128        |     >= 128    |
            //
            tmp = _mm_subs_epu8(pos, _mm_set1_epi8(0xF0));
            range2 = _mm_shuffle_epi8(xDfxEeTable, tmp);
            tmp = _mm_adds_epu8(pos, _mm_set1_epi8(0x70));
            range2 = _mm_add_epi8(range2, _mm_shuffle_epi8(xEfxFeTable, tmp));

            range = _mm_add_epi8(range, range2);

            // Load min and max values per calculated range index
            vec128i minv = _mm_shuffle_epi8(rangeMinTable, range);
            vec128i maxv = _mm_shuffle_epi8(rangeMaxTable, range);

            // Check value range
            // error |= (input < minv) | (input > maxv)
            tmp = _mm_or_si128(
                _mm_cmplt_epi8(input, minv),
                _mm_cmpgt_epi8(input, maxv)
            );

            error = _mm_or_si128(error, tmp);

            prevInput = input;
            prevFirstLength = firstLength;

            source += 16;
            length -= 16;
        }

        if (!_mm_testz_si128(error, error)) return -1;

        // Find previous token (not 80~BF)
        int32 token = _mm_extract_epi32(prevInput, 3);
        
        // Split an int32 into 4 int8
        const int8 *tokens = (const int8 *)&token;

        int lookahead = 0;

        if (tokens[3] > (int8)0xBF)
        {
            lookahead = 1;
        }
        else if (tokens[2] > (int8)0xBF)
        {
            lookahead = 2;
        }
        else if (tokens[1] > (int8)0xBF)
        {
            lookahead = 3;
        }

        source -= lookahead;
        length += lookahead;
    }

    // Check remaining bytes with the scalar algorithm
    return u8ScalarAlgorithm(source, length);
}

#endif
