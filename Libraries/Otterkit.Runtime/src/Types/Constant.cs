using System.Text;

namespace Otterkit.Runtime;

public readonly struct Constant : ICOBOLType
{
    public readonly OtterMemory Memory;
    public readonly int Length;

    public Constant(ReadOnlySpan<byte> bytes)
    {
        Length = bytes.Length;

        Memory = new OtterMemory(Length);

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
