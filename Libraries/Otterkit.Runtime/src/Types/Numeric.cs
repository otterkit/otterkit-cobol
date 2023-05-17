using System.Text;
using Otterkit.Numerics;

namespace Otterkit.Runtime;

public readonly struct Numeric : ICOBOLType, IComparable<Numeric>
{
    public readonly Memory<byte> Memory { get; init; }
    public readonly int Offset;
    public readonly int Length;
    public readonly int FractionalLength;

    public readonly bool IsInteger;
    public readonly bool IsSigned;
    public readonly bool IsNegative
    {
        get => Memory.Span[0] is 45;
    }

    public Numeric(ReadOnlySpan<byte> value, int offset, int length, int fractionalLength, Memory<byte> memory)
    {
        Offset = offset;
        Length = length;
        FractionalLength = fractionalLength;

        IsInteger = fractionalLength == 0;
        IsSigned = value[0] is 43 or 45;

        if (fractionalLength == 0)
        {
            int signedSpace = IsSigned ? 1 : 0;
            Memory = memory.Slice(offset, length + signedSpace);
        }

        if (fractionalLength > 0)
        {
            int signedSpace = IsSigned ? 2 : 1;
            Memory = memory.Slice(offset, length + fractionalLength + signedSpace);
        }

        Memory.Span.Fill(48);

        if (IsSigned)
        {
            FormatSigned(value);
            return;
        }

        Format(value);
    }

    public Numeric(Memory<byte> memory, int offset, int length, int fractionalLength, bool isSigned)
    {
        Offset = offset;
        Length = length;
        FractionalLength = fractionalLength;

        IsSigned = isSigned;
        IsInteger = fractionalLength == 0;

        if (fractionalLength == 0)
        {
            int signedSpace = isSigned ? 1 : 0;
            Memory = memory.Slice(offset, length + signedSpace);
        }

        if (fractionalLength > 0)
        {
            int signedSpace = isSigned ? 2 : 1;
            Memory = memory.Slice(offset, length + fractionalLength + signedSpace);
        }
    }

    public Numeric(ReadOnlySpan<byte> utf8String, bool isSigned)
    {
        Offset = 0;

        int DecimalPointIndex = utf8String.IndexOf("."u8);

        if (DecimalPointIndex >= 0)
        {
            int minusSignOffset = utf8String[0] != 45 ? 0 : 1;

            Length = isSigned ? DecimalPointIndex - minusSignOffset : DecimalPointIndex;
            FractionalLength = isSigned ? utf8String.Length - Length - (minusSignOffset + 1) : utf8String.Length - Length - 1;
        }
        else
        {
            if (utf8String[0] is 45 or 43)
            {
                Length = utf8String.Length - 1;
            }
            else
            {
                Length = utf8String.Length;
            }

            FractionalLength = 0;
        }

        IsSigned = isSigned;
        IsInteger = FractionalLength == 0;

        if (FractionalLength == 0)
        {
            int signedSpace = isSigned ? 1 : 0;
            Memory = new byte[Length + signedSpace];
        }

        if (FractionalLength > 0)
        {
            int signedSpace = isSigned ? 2 : 1;
            Memory = new byte[Length + FractionalLength + signedSpace];
        }

        Memory.Span.Fill(48);

        if (isSigned)
        {
            FormatSigned(utf8String);
            return;
        }

        Format(utf8String);
    }

    private void Format(ReadOnlySpan<byte> bytes, bool isSigned = false)
    {
        Span<byte> formatted = stackalloc byte[Memory.Length];
        formatted.Fill(48);

        int indexOfDecimal = bytes.IndexOf("."u8);

        if (!IsInteger && indexOfDecimal < 0)
        {
            indexOfDecimal = bytes.Length;
            formatted[Length + (IsSigned ? 1 : 0)] = 46;
        }

        int startIndex;
        if (!IsInteger || indexOfDecimal > -1)
        {
            startIndex = Math.Max(0, indexOfDecimal - Length);
        }
        else
        {
            startIndex = Math.Max(0, bytes.Length - Length);
        }

        int endIndex;
        if (!IsInteger)
        {
            endIndex = Math.Min(bytes.Length - startIndex, Length + FractionalLength + 1);
        }
        else
        {
            endIndex = indexOfDecimal < 0
            ? Math.Min(Length, bytes.Length)
            : Math.Min(Length, indexOfDecimal);
        }

        int offset;
        if (indexOfDecimal < 0)
        {
            offset = Math.Max(0, Memory.Length - bytes.Length);
        }
        else
        {
            offset = Math.Max(0, Length - indexOfDecimal);
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

    public static implicit operator Numeric(Decimal128 value)
    {
        Span<byte> span = stackalloc byte[43];

        var length = value.AsSpan(span);

        return new Numeric(span.Slice(0, length), true);
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

    public static Numeric operator +(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        Decimal128 Dres = Ldec + Rdec;

        return Dres;
    }

    public static Numeric operator ++(Numeric number)
    {
        Decimal128 num = number;

        num = num++;

        return num;
    }

    public static Numeric operator -(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        Decimal128 Dres = Ldec - Rdec;

        return Dres;
    }

    public static Numeric operator -(Numeric number)
    {
        Decimal128 num = number;

        num = -num;

        return num;
    }

    public static Numeric operator --(Numeric number)
    {
        Decimal128 num = number;

        num = num--;

        return num;
    }

    public static Numeric operator *(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        Decimal128 Dres = Ldec * Rdec;

        return Dres;
    }

    public static Numeric operator /(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        Decimal128 Dres = Ldec / Rdec;

        return Dres;
    }

    public static Numeric operator %(Numeric left, Numeric right)
    {
        Decimal128 Ldec = left;
        Decimal128 Rdec = right;

        Decimal128 Dres = Ldec % Rdec;

        return Dres;
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
