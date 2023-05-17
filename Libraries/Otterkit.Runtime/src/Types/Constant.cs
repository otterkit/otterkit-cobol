using System.Text;

namespace Otterkit.Runtime;

public readonly struct Constant : ICOBOLType
{
    public readonly Memory<byte> Memory { get; init; }
    public readonly int Length;

    public Constant(ReadOnlySpan<byte> bytes)
    {
        Length = bytes.Length;

        Memory = new byte[Length];

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
