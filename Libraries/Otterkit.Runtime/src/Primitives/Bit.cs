using System.Text;

namespace Otterkit.Runtime;

public readonly struct Bit : ICOBOLType
{
    public readonly OtterMemory Memory { get; init; }
    public readonly int Offset;
    public readonly int Length;

    public Bit(ReadOnlySpan<byte> value, OtterMemory memory, int offset, int length)
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

    public Bit(OtterMemory memory, int offset, int length)
    {
        Length = length;
        Offset = offset;

        Memory = memory;
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
