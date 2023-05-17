using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

[StructLayout(LayoutKind.Sequential)]
public readonly partial struct Decimal128 :
    IStandardDecimal<Decimal128>
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
    
    public int CompareTo(object? obj)
    {
        if (obj is null) return 1;
        
        if (obj is Decimal128 decQuad)
        {
            return CompareTo(decQuad);
        }
        
        throw new ArgumentException("CompareTo argument must be a Decimal128", nameof(obj));
    }

    public int CompareTo(Decimal128 other)
    {
        // Call decQuad compare function.
        var compare = DecQuadBindings.Compare(this, other);

        // decQuad bindings will only return -5 if one
        // of the arguments is NaN.
        if (compare is not -5) return compare;

        // Determine which one is NaN.
        return IsNaN(this) ? (IsNaN(other) ? 0 : -1) : 1;
    }

    public override int GetHashCode()
    {
        // Does this work with this encoding?
        return HashCode.Combine(_upperBits, _lowerBits);
    }

    public static (ulong UpperBits, ulong LowerBits) Encode(Decimal128 value)
    {
        return (value._upperBits, value._lowerBits);
    }

    public static Decimal128 Decode(ulong upperBits, ulong lowerBits)
    {
        return new Decimal128(upperBits, lowerBits);
    }
}
