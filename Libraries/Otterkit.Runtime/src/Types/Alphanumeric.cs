using System.Text;

namespace Otterkit.Runtime;

public readonly struct Alphanumeric : ICOBOLType
{
    public readonly Memory<byte> Memory { get; init; }
    public readonly int Length;
    private readonly int Offset;

    public Alphanumeric(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory, ICOBOLType[]? fields = null)
    {
        Length = length;
        Offset = offset;

        Memory = memory.Slice(offset, length);
 
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value[..byteLength].CopyTo(Memory.Span);
    }

    public Alphanumeric(Memory<byte> memory, int offset, int length, ICOBOLType[]? fields = null)
    {
        Length = length;
        Offset = offset;

        Memory = memory.Slice(offset, length);
    }

    public Alphanumeric(ReadOnlySpan<byte> alphanumeric)
    {
        Length = alphanumeric.Length;
        
        Memory = new byte[Length];

        alphanumeric.CopyTo(Memory.Span);
    }

    public static bool operator >(Alphanumeric left, Alphanumeric right)
    {
        int unequalPosition = left.Bytes.CommonPrefixLength(right.Bytes);
        return left.Bytes[unequalPosition] > right.Bytes[unequalPosition];
    }

    public static bool operator <(Alphanumeric left, Alphanumeric right)
    {
        int unequalPosition = left.Bytes.CommonPrefixLength(right.Bytes);
        return left.Bytes[unequalPosition] < right.Bytes[unequalPosition];
    }

    public static bool operator >=(Alphanumeric left, Alphanumeric right)
    {
        int unequalPosition = left.Bytes.CommonPrefixLength(right.Bytes);
        return left.Bytes[unequalPosition] >= right.Bytes[unequalPosition];
    }

    public static bool operator <=(Alphanumeric left, Alphanumeric right)
    {
        int unequalPosition = left.Bytes.CommonPrefixLength(right.Bytes);
        return left.Bytes[unequalPosition] <= right.Bytes[unequalPosition];
    }

    public static bool operator ==(Alphanumeric left, Alphanumeric right)
    {
        return left.Bytes.SequenceEqual(right.Bytes);
    }

    public static bool operator !=(Alphanumeric left, Alphanumeric right)
    {
        return !left.Bytes.SequenceEqual(right.Bytes);
    }

    public override bool Equals(object? obj)
    {
        throw new NotSupportedException();
    }

    public override int GetHashCode()
    {
        throw new NotSupportedException();
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            Memory.Span.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value[..length].CopyTo(Memory.Span);
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
