using System.Text;
using Otterkit.Numerics;

namespace Otterkit.Runtime;

public readonly struct Numeric : ICOBOLType, INumeric, IComparable<Numeric>
{
    public readonly Memory<byte> Memory;
    public readonly int Offset;

    public readonly (int Integer, int Fractional) Length { get; init; }

    public readonly bool IsInteger { get; init; }
    public readonly bool IsSigned { get; init; }
    public readonly bool IsNegative
    {
        get => Memory.Span[0] is 45;
    }

    public Numeric(ReadOnlySpan<byte> value, Memory<byte> memory, int offset, int integer, int fractional)
    {
        Offset = offset;
        Length = (integer, fractional);

        IsInteger = fractional == 0;
        IsSigned = value[0] is 43 or 45;

        if (fractional == 0)
        {
            int signedSpace = IsSigned ? 1 : 0;
            Memory = memory.Slice(offset, integer + signedSpace);
        }

        if (fractional > 0)
        {
            int signedSpace = IsSigned ? 2 : 1;
            Memory = memory.Slice(offset, integer + fractional + signedSpace);
        }

        Memory.Span.Fill(48);

        if (IsSigned)
        {
            FormatSigned(value);
            return;
        }

        Format(value);
    }

    public Numeric(Memory<byte> memory, int offset, int integer, int fractional, bool isSigned)
    {
        Offset = offset;
        Length = (integer, fractional);

        IsSigned = isSigned;
        IsInteger = fractional == 0;

        if (fractional == 0)
        {
            int signedSpace = isSigned ? 1 : 0;
            Memory = memory.Slice(offset, integer + signedSpace);
        }

        if (fractional > 0)
        {
            int signedSpace = isSigned ? 2 : 1;
            Memory = memory.Slice(offset, integer + fractional + signedSpace);
        }
    }

    private void Format(ReadOnlySpan<byte> bytes, bool isSigned = false)
    {
        var (integer, fractional) = Length;

        Span<byte> formatted = stackalloc byte[Memory.Length];
        formatted.Fill(48);

        int indexOfDecimal = bytes.IndexOf("."u8);

        if (!IsInteger && indexOfDecimal < 0)
        {
            indexOfDecimal = bytes.Length;
            formatted[integer + (IsSigned ? 1 : 0)] = 46;
        }

        int startIndex;
        if (!IsInteger || indexOfDecimal > -1)
        {
            startIndex = Math.Max(0, indexOfDecimal - integer);
        }
        else
        {
            startIndex = Math.Max(0, bytes.Length - integer);
        }

        int endIndex;
        if (!IsInteger)
        {
            endIndex = Math.Min(bytes.Length - startIndex, integer + fractional + 1);
        }
        else
        {
            endIndex = indexOfDecimal < 0
            ? Math.Min(integer, bytes.Length)
            : Math.Min(integer, indexOfDecimal);
        }

        int offset;

        if (indexOfDecimal < 0)
        {
            offset = Math.Max(0, Memory.Length - bytes.Length);
        }
        else
        {
            offset = Math.Max(0, integer - indexOfDecimal);
        }

        if (isSigned)
        {
            offset++;
        }

        endIndex += startIndex;

        bytes[startIndex..endIndex].CopyTo(formatted[offset..]);

        int indexOfSign = formatted.IndexOfAny("+-"u8);
        if (indexOfSign > -1) formatted[indexOfSign] = 48;

        if (!isSigned)
        {
            formatted.CopyTo(Memory.Span);
            return;
        }

        formatted.CopyTo(Memory.Span);
        Memory.Span[0] = (byte)(IsNegative ? 45 : 43);
    }

    private void FormatSigned(ReadOnlySpan<byte> bytes)
    {
        if (bytes[0] is 45 or 43)
        {
            Format(bytes, true);

            return;
        }

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
            return Memory.Span;
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
            return Encoding.UTF8.GetString(Memory.Span);
        }
    }
}
