using System.Runtime.InteropServices;

namespace Otterkit.Runtime;

public static unsafe partial class u8Strings
{
    [LibraryImport("nativelib", EntryPoint = "u8CharFromCodepoint")]
    private static partial int FromCodepoint(uint codepoint, byte* destination);

    [LibraryImport("nativelib", EntryPoint = "u8CharToCodepoint")]
    private static partial int ToCodepoint(byte* source, uint* destination);

    public static int FromCodepoint(uint codepoint, Span<byte> destination)
    {
        fixed (byte* ptr = destination)
        {
            return FromCodepoint(codepoint, ptr);
        }
    }

    public static int ToCodepoint(ReadOnlySpan<byte> source, Span<uint> destination)
    {
        fixed (byte* ptr = source)
        fixed (uint* dest = destination)
        {
            return ToCodepoint(ptr, dest);
        }
    }
}