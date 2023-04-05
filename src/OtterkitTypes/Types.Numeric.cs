using System.Text;
using Otterkit.Numerics;

namespace Otterkit.Library;

public sealed class Numeric : ICOBOLType, IComparable<Numeric>
{
    public Memory<byte> Memory { get; init; }
    public ICOBOLType[] Fields { get; init; } 
    public int Offset { get; init; }
    public int Length { get; init; }
    public int FractionalLength { get; init; }
    public bool IsInteger { get; private set; }
    public bool IsSigned { get; private set; }
    public bool IsNegative { get; private set; }

    public Numeric(ReadOnlySpan<byte> value, int offset, int length, int fractionalLength, Memory<byte> memory)
    {
        this.Fields = Array.Empty<ICOBOLType>();
        this.IsSigned = value[0] == 43 || value[0] == 45;
        this.Offset = offset;
        this.Length = length;
        this.FractionalLength = fractionalLength;
        this.IsInteger = fractionalLength == 0;

        if (fractionalLength == 0)
        {
            int signedSpace = IsSigned ? 1 : 0;
            this.Memory = memory.Slice(offset, length + signedSpace);
        }

        if (fractionalLength > 0)
        {
            int signedSpace = IsSigned ? 2 : 1;
            this.Memory = memory.Slice(offset, length + fractionalLength + signedSpace);
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
        this.Fields = Array.Empty<ICOBOLType>();
        this.Offset = offset;
        this.Length = length;
        this.FractionalLength = fractionalLength;
        this.IsSigned = isSigned;
        this.IsInteger = fractionalLength == 0;

        if (fractionalLength == 0)
        {
            int signedSpace = isSigned ? 1 : 0;
            this.Memory = memory.Slice(offset, length + signedSpace);
        }

        if (fractionalLength > 0)
        {
            int signedSpace = isSigned ? 2 : 1;
            this.Memory = memory.Slice(offset, length + fractionalLength + signedSpace);
        }
    }

    public Numeric(DecimalHolder decimalHolder, bool isSigned)
    {
        this.Fields = Array.Empty<ICOBOLType>();
        this.Offset = 0;

        int DecimalPointIndex = decimalHolder.Bytes.IndexOf("."u8);

        if (DecimalPointIndex >= 0)
        {
            int minusSignOffset = decimalHolder.Bytes[0] != 45 ? 0 : 1;

            this.Length = isSigned ? DecimalPointIndex - minusSignOffset : DecimalPointIndex;
            this.FractionalLength = isSigned ? decimalHolder.Bytes.Length - Length - (minusSignOffset + 1) : decimalHolder.Bytes.Length - Length - 1;
        }
        else
        {
            if (decimalHolder.Bytes[0] is 45 or 43)
            {
                this.Length = decimalHolder.Bytes.Length - 1;
            }
            else
            {
                this.Length = decimalHolder.Bytes.Length;
            }

            this.FractionalLength = 0;
        }

        this.IsSigned = isSigned;
        this.IsInteger = FractionalLength == 0;

        if (this.FractionalLength == 0)
        {
            int signedSpace = isSigned ? 1 : 0;
            this.Memory = new byte[Length + signedSpace];
        }

        if (this.FractionalLength > 0)
        {
            int signedSpace = isSigned ? 2 : 1;
            this.Memory = new byte[Length + FractionalLength + signedSpace];
        }

        Memory.Span.Fill(48);

        if (isSigned)
        {
            FormatSigned(decimalHolder.Bytes);
            return;
        }

        Format(decimalHolder.Bytes);
    }

    public Numeric(ReadOnlySpan<byte> utf8String, bool isSigned)
    {
        this.Fields = Array.Empty<ICOBOLType>();
        this.Offset = 0;

        int DecimalPointIndex = utf8String.IndexOf("."u8);

        if (DecimalPointIndex >= 0)
        {
            int minusSignOffset = utf8String[0] != 45 ? 0 : 1;

            this.Length = isSigned ? DecimalPointIndex - minusSignOffset : DecimalPointIndex;
            this.FractionalLength = isSigned ? utf8String.Length - Length - (minusSignOffset + 1) : utf8String.Length - Length - 1;
        }
        else
        {
            if (utf8String[0] is 45 or 43)
            {
                this.Length = utf8String.Length - 1;
            }
            else
            {
                this.Length = utf8String.Length;
            }

            this.FractionalLength = 0;
        }

        this.IsSigned = isSigned;
        this.IsInteger = FractionalLength == 0;

        if (this.FractionalLength == 0)
        {
            int signedSpace = isSigned ? 1 : 0;
            this.Memory = new byte[Length + signedSpace];
        }

        if (this.FractionalLength > 0)
        {
            int signedSpace = isSigned ? 2 : 1;
            this.Memory = new byte[Length + FractionalLength + signedSpace];
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
        if (bytes[0] == 45)
        {
            IsNegative = true;
            Format(bytes, true);
            return;
        }

        if (bytes[0] != 43)
        {
            Span<byte> withSign = stackalloc byte[bytes.Length + 1];
            bytes.CopyTo(withSign[1..]);
            withSign[0] = 43;
            IsNegative = false;
            Format(withSign, true);
            return;
        }

        IsNegative = false;
        Format(bytes, true);
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
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec > Rdec);
    }

    public static bool operator <(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec < Rdec);
    }

    public static bool operator <=(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec <= Rdec);
    }

    public static bool operator >=(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec >= Rdec);
    }

    public static bool operator ==(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec == Rdec);
    }

    public static bool operator !=(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec != Rdec);
    }

    public static Numeric operator +(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec + Rdec;

        Numeric result = new(Dres, true);

        return result;
    }

    public static Numeric operator ++(Numeric number)
    {
        DecimalHolder num = number;

        num = num++;

        Numeric result = new(num, true);

        return result;
    }

    public static Numeric operator -(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec - Rdec;

        Numeric result = new(Dres, true);

        return result;
    }

    public static Numeric operator -(Numeric number)
    {
        DecimalHolder num = number;

        num = -num;

        Numeric result = new(num, true);

        return result;
    }

    public static Numeric operator --(Numeric number)
    {
        DecimalHolder num = number;

        num = num--;

        Numeric result = new(num, true);

        return result;
    }

    public static Numeric operator *(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec * Rdec;

        Numeric result = new(Dres, true);

        return result;
    }

    public static Numeric operator /(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec / Rdec;

        Numeric result = new(Dres, true);

        return result;
    }

    public static Numeric operator %(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec % Rdec;

        Numeric result = new(Dres, true);

        return result;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null || GetType() != obj.GetType())
        {
            return false;
        }
        
        return this == (Numeric)obj;
    }

    public int CompareTo(Numeric? other)
    { 
        //for implementing default C# sorting
        if (other is null || this > other) return 1;
        
        if (this.Equals(other)) return 0;
         
        return -1;
    }
    
    public override int GetHashCode()
    {
        return Memory.Span[0].GetHashCode();
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
