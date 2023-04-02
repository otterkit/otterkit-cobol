using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

[StructLayout(LayoutKind.Sequential)]
public readonly partial struct DecimalQuad
{
    public readonly ulong _upperBits;
    public readonly ulong _lowerBits;

    public DecimalQuad(ulong upperBits, ulong lowerBits)
    {
        _upperBits = upperBits;
        _lowerBits = lowerBits;
    }

    public DecimalQuad(ReadOnlySpan<byte> utf8String)
    {
        this = DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public DecimalQuad(ReadOnlySpan<char> characters)
    {
        var length = Encoding.UTF8.GetByteCount(characters);

        Span<byte> utf8String = stackalloc byte[length];

        Encoding.UTF8.GetBytes(characters, utf8String);
        
        this = DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }
}
