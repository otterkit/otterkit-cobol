using System.Runtime.CompilerServices;
using System.Text;
using Otterkit.Numerics;

namespace Otterkit.Runtime;

public readonly struct Numeric : ICOBOLType, INumeric, IComparable<Numeric>
{
    public readonly OtterMemory Memory;
    public readonly int Offset;

    public readonly byte Extra { get; init; }
    public readonly byte Integer { get; init; }
    public readonly byte Fractional { get; init; }
    public readonly bool IsInteger { get; init; }
    public readonly bool IsSigned { get; init; }
    public readonly bool IsNegative => Memory.Span[0] is 45;

    public Numeric(ReadOnlySpan<byte> value, OtterMemory memory, int offset, byte integer, byte fractional)
    {
        Offset = offset;
        Integer = integer;
        Fractional = fractional;
        IsInteger = fractional == 0;
        IsSigned = value[0] is 43 or 45;

        Extra = SignBytes();

        Memory = memory;

        if (IsSigned)
        {
            FormatSigned(value);
            return;
        }

        Format(value);
    }

    public Numeric(OtterMemory memory, int offset, byte integer, byte fractional, bool isSigned)
    {
        Offset = offset;
        Integer = integer;
        IsSigned = isSigned;
        Fractional = fractional;
        IsInteger = fractional == 0;

        Extra = SignBytes();

        Memory = memory;
    }

    private Span<byte> Span => Memory.Span[Offset..(Extra + Integer + Fractional)];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private byte SignBytes()
    {
        if (Fractional == 0 && IsSigned) return 1;

        if (Fractional == 0 && !IsSigned) return 0;

        if (IsSigned) return 2;

        return 1;
    }

    /// <summary>
    /// Formats the given UTF-8 string into the picture representation of the numeric value.
    /// </summary>
    /// <param name="bytes">The UTF-8 string to format.</param>
    /// <param name="isSigned">Whether the numeric value is signed or not.</param>
    private void Format(ReadOnlySpan<byte> bytes, bool isSigned = false)
    {
        // Fetch the fixed lengths of the numeric value
        // Separated into {INTEGER . FRACTIONAL} values.
        var integer = Integer;
        var fractional = Fractional;

        // The total length of the numeric varible's memory.
        var memoryLength = Span.Length;

        // Create a new span to hold the formatted value
        // stackalloc is used to avoid heap allocations.
        Span<byte> formatted = stackalloc byte[memoryLength];

        // Fill the formatted span with UTF-8 '0's (byte 48).
        formatted.Fill(48);

        // Find the decimal point in the string.
        var decimalPoint = bytes.IndexOf("."u8);

        // Get the length of the string.
        var length = bytes.Length;

        // If the numeric variable is not an integer
        // and the decimal point is missing in the string.
        if (!IsInteger && decimalPoint < 0)
        {
            // Decimal point is set to the length of the string
            // because it's after the last digit (the string represents an integer).
            decimalPoint = length;

            // Add the decimal point to the formatted span
            // UTF-8 decimal point is 46...
            formatted[integer + (IsSigned ? 1 : 0)] = 46;
        }

        int start;

        int value;
        // Determine the starting position of the string
        // for truncation purposes.
        if (!IsInteger || decimalPoint > -1)
        {
            // If this is not negative, the integer length in the string
            // is within the integer length of the numeric variable.
            value = decimalPoint - integer;
        }
        else
        {
            // Same as above, but the numeric variable and the string
            // are both integers.
            value = length - integer;
        }

        // If the value is negative, set the start position to 0.
        // Integer length in the string is not within the 
        // integer length of the numeric variable.
        start = Math.Max(0, value);

        int end;

        // Determine the ending position of the string
        // for truncation purposes.
        if (!IsInteger)
        {
            // The total length of the numeric variable
            // including the decimal point.
            var totalLength = Integer + Fractional + 1;

            // The length of the string minus the starting position.
            var valueLength = length - start;

            // If the string is too big to fit in the numeric variable
            // then the ending position will truncate the string.
            end = Math.Min(totalLength, valueLength);
        }
        else
        {
            // Same as the above, but both are integers.
            // If the string has a decimal point, the ending position
            // will only keep the fractional part of the string.
            if (decimalPoint < 0)
            {
                // Check if the string is too big to fit in the numeric variable.
                end = Math.Min(integer, length);
            }
            else
            {
                // Check if the integer part of the string is too big to fit
                // in the numeric variable.
                end = Math.Min(integer, decimalPoint);
            }
        }

        // The position of the string in the formatted span.
        int offset;

        if (decimalPoint < 0)
        {
            value = memoryLength - length;
        }
        else
        {
            value = integer - decimalPoint;
        }

        // If the value is negative, set the offset to 0
        // because the integer length in the string is not
        // within the integer length of the numeric variable.
        offset = Math.Max(0, value);

        if (isSigned)
        {
            // Plus 1 because the sign byte is in the first position.
            // This is to leave room for the sign byte at the start.
            offset++;
        }
        
        end += start;

        // Copy the truncated part of the string to the formatted span
        // starting from the offset position.
        bytes[start..end].CopyTo(formatted[offset..]);

        // If the formatted span contains a sign character, replace it with '0'
        // because the sign might be in the wrong position (middle of the string).
        int signIndex = formatted.IndexOfAny("+-"u8);

        // UTF-8 '0' is 48...
        if (signIndex > -1) formatted[signIndex] = 48;

        // Copy the formatted span to the varible's memory.
        formatted.CopyTo(Span);

        // If the numeric variable is signed, also copy the sign byte
        // to the varible's memory.
        if (isSigned) Memory.Span[0] = bytes[0];
    }

    private void FormatSigned(ReadOnlySpan<byte> bytes)
    {
        // If the first byte of the string is a plus or minus sign, format the string as-is.
        if (bytes[0] is 45 or 43)
        {
            Format(bytes, true);
            return;
        }

        // If the first byte of the string is not a sign, add a plus sign to the beginning of the string and format it.
        Span<byte> withSign = stackalloc byte[bytes.Length + 1];

        bytes.CopyTo(withSign[1..]);
        
        withSign[0] = 43;
        
        Format(withSign, true);
    }

    public static implicit operator Decimal128(Numeric value)
    {
        return Decimal128.Parse(value.Bytes);
    }

    public static bool operator >(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        return Ldec > Rdec;
    }

    public static bool operator <(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        return Ldec < Rdec;
    }

    public static bool operator <=(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        return Ldec <= Rdec;
    }

    public static bool operator >=(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        return Ldec >= Rdec;
    }

    public static bool operator ==(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        return Ldec == Rdec;
    }

    public static bool operator !=(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        return Ldec != Rdec;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null || GetType() != obj.GetType())
        {
            return false;
        }

        return this == (Numeric)obj;
    }

    public bool Equals(Numeric obj)
    {
        return this == obj;
    }

    public int CompareTo(Numeric other)
    {
        //for implementing default C# sorting
        if (this > other) return 1;

        if (Equals(other)) return 0;

        return -1;
    }

    public override int GetHashCode()
    {
        return Memory.GetHashCode();
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Span;
        }
        set
        {
            if (IsSigned)
            {
                FormatSigned(value);
                return;
            }

            Format(value);
        }
    }

    public string Display
    {
        get
        {
            return Encoding.UTF8.GetString(Span);
        }
    }
}
