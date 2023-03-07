using System.Text;

namespace OtterkitLibrary;

public sealed class Constant : ICOBOLType
{
    public Memory<byte> Memory { get; init; }
    public ICOBOLType[] Fields { get; init; }
    public int Length { get; init; }

    public Constant(ReadOnlySpan<byte> bytes)
    {
        this.Length = bytes.Length;
        this.Memory = new byte[Length];
        this.Fields = Array.Empty<ICOBOLType>();

        bytes.CopyTo(Memory.Span);
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            return;
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
