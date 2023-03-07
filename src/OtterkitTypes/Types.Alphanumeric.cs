using System.Text;

namespace OtterkitLibrary;

public sealed class Alphanumeric : ICOBOLType
{
    public Memory<byte> Memory { get; init; }
    public ICOBOLType[] Fields { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }

    public Alphanumeric(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory, ICOBOLType[]? fields = null)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        this.Fields = fields ?? Array.Empty<ICOBOLType>();
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value[..byteLength].CopyTo(Memory.Span);
    }

    public Alphanumeric(Memory<byte> memory, int offset, int length, ICOBOLType[]? fields = null)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        this.Fields = fields ?? Array.Empty<ICOBOLType>();
    }

    public Alphanumeric(ReadOnlySpan<byte> alphanumeric)
    {
        this.Length = alphanumeric.Length;
        this.Memory = new byte[Length];
        this.Fields = Array.Empty<ICOBOLType>();
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
