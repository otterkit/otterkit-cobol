using System.Text;

namespace Otterkit.Runtime;

public readonly struct Alphanumeric : ICOBOLType
{
    public readonly OtterMemory Memory { get; init; }
    public readonly int Length;
    private readonly int Offset;

    public Alphanumeric(ReadOnlySpan<byte> value, OtterMemory memory, int offset, int length)
    {
        Length = length;
        Offset = offset;

        Memory = memory;

        var span = Memory.Slice(offset, length);
 
        span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value[..byteLength].CopyTo(span);
    }

    public Alphanumeric(OtterMemory memory, int offset, int length)
    {
        Length = length;
        Offset = offset;

        Memory = memory;
    }

    public Alphanumeric(ReadOnlySpan<byte> alphanumeric)
    {
        Length = alphanumeric.Length;
        
        Memory = new OtterMemory(Length);

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
            return Memory.Slice(Offset, Length);
        }
        set
        {
            var span = Memory.Slice(Offset, Length);

            span.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value[..length].CopyTo(span);
        }
    }

    public string Display
    {
        get
        {
            var span = Memory.Slice(Offset, Length);

            return Encoding.UTF8.GetString(span);
        }
    }
}
