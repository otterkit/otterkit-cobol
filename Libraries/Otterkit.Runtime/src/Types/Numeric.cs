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
        Fractional = fractional;

        IsSigned = isSigned;
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

    private void Format(ReadOnlySpan<byte> bytes, bool isSigned = false)
    {
        var integer = Integer;
        var fractional = Fractional;

        var memoryLength = Span.Length;

        Span<byte> formatted = stackalloc byte[memoryLength];

        formatted.Fill(48);

        var indexOfDecimal = bytes.IndexOf("."u8);

        var length = bytes.Length;

        if (!IsInteger && indexOfDecimal < 0)
        {
            indexOfDecimal = length;

            formatted[integer + (IsSigned ? 1 : 0)] = 46;
        }

        int start;

        if (!IsInteger || indexOfDecimal > -1)
        {
            var value = indexOfDecimal - integer;

            start = 0 >= value ? 0 : value;
        }
        else
        {
            var value = length - integer;

            start = 0 >= value ? 0 : value;
        }

        int end;

        if (!IsInteger)
        {
            var left = length - start;
            var right = integer + fractional + 1;

            end = left <= right ? left : right;
        }
        else
        {
            end = indexOfDecimal < 0
            ? integer <= length 
                ? integer 
                : length
            : integer <= indexOfDecimal 
                ? integer 
                : indexOfDecimal;
        }

        int offset;

        if (indexOfDecimal < 0)
        {
            var value = memoryLength - length;

            offset = 0 >= value ? 0 : value;
        }
        else
        {
            var value = integer - indexOfDecimal;

            offset = 0 >= value ? 0 : value;
        }

        if (isSigned) offset++;

        end += start;

        bytes[start..end].CopyTo(formatted[offset..]);

        int sign = formatted.IndexOfAny("+-"u8);

        if (sign > -1) formatted[sign] = 48;

        formatted.CopyTo(Span);

        if (!isSigned) return;

        Memory.Span[0] = bytes[0];
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
