using System.Text;

namespace Otterkit.Runtime;

public unsafe struct Alphanumeric : ICOBOLType, IDisposable
{
    private byte* Memory;
    private uint Combined;

    public uint Length => Combined.ClearBit(31);
    public uint Dynamic => Combined.FetchBit(31);

    private Span<byte> Span => new(Memory, (int)Length);

    public Alphanumeric(ReadOnlySpan<byte> value, byte* memory, uint length)
    {
        Memory = memory;
        
        Combined = length;

        var span = Span;
 
        span.Fill(32);

        int byteLength = Length < value.Length
            ? (int)Length
            : value.Length;

        value[..byteLength].CopyTo(span);
    }

    public Alphanumeric(byte* memory, uint length)
    {
        Memory = memory;
        Combined = length;
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

    public void Dispose()
    {
        throw new NotImplementedException();
    }

    public ReadOnlySpan<byte> Bytes
    {
        get => Span;
        
        set
        {
            var span = Span;

            span.Fill(32);

            int length = Length < value.Length
            ? (int)Length
            : value.Length;

            value[..length].CopyTo(span);
        }
    }

    public string Display => Encoding.UTF8.GetString(Span);
}
