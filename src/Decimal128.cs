using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

[StructLayout(LayoutKind.Sequential)]
public readonly partial struct Decimal128
{
    internal readonly ulong _upperBits;
    internal readonly ulong _lowerBits;

    public Decimal128(ulong upperBits, ulong lowerBits)
    {
        _upperBits = upperBits;
        _lowerBits = lowerBits;
    }

    public Decimal128(ReadOnlySpan<byte> utf8String)
    {
        this = DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public Decimal128(ReadOnlySpan<char> characters)
    {
        var length = Encoding.UTF8.GetByteCount(characters);

        Span<byte> utf8String = stackalloc byte[length];

        Encoding.UTF8.GetBytes(characters, utf8String);
        
        this = DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj == null || GetType() != obj.GetType())
        {
            return false;
        }
        
        if (obj is Decimal128 decQuad)
        {
            return Equals(decQuad);
        }

        return false;
    }

    public bool Equals(Decimal128 decQuad)
    {
        return DecQuadBindings.Compare(this, decQuad) == 0;
    }
    
    public override int GetHashCode()
    {
        return HashCode.Combine(_upperBits, _lowerBits);
    }
}
