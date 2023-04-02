using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;


[StructLayout(LayoutKind.Sequential)]
public readonly struct DecimalQuad
{
    internal readonly ulong _upperBits;
    internal readonly ulong _lowerBits;

    public DecimalQuad(ReadOnlySpan<byte> utf8String)
    {
        this = DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public DecimalQuad(ulong upperBits, ulong lowerBits)
    {
        _upperBits = upperBits;
        _lowerBits = lowerBits;
    }

    public static DecimalQuad Add(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Add(left, right);
    }

    public static DecimalQuad Subtract(DecimalQuad left, DecimalQuad right)
    {
        return DecQuadBindings.Subtract(left, right);
    }

    public DecimalQuad FromString(ReadOnlySpan<byte> utf8String)
    {
        return DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public override unsafe string ToString()
    {
        var pointer = DecQuadBindings.ToString(this);

        int length = 0;
        byte current = pointer[0];

        while (current != 0)
        {
            current = pointer[length];
            length++;
        }

        var outString = Encoding.UTF8.GetString(pointer, length);

        return outString;
    }
}

internal static partial class DecQuadBindings
{
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadFromString")]
    internal static partial DecimalQuad FromString(in byte value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadToString")]
    internal static unsafe partial byte* ToString(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadAdd")]
    internal static partial DecimalQuad Add(DecimalQuad left, DecimalQuad right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadSub")]
    internal static partial DecimalQuad Subtract(DecimalQuad left, DecimalQuad right);
}
