using System.Runtime.InteropServices;

namespace Otterkit.Native;

public static unsafe partial class u8Console
{
    public static void WriteLine(ReadOnlySpan<byte> format, ReadOnlySpan<byte> utf8)
    {
        Span<byte> styled = stackalloc byte[format.Length + utf8.Length];

        format.CopyTo(styled);

        utf8.CopyTo(styled.Slice(format.Length));

        WriteLine(styled);
    }

    public static void WriteLine(ReadOnlySpan<byte> utf8)
    {
        Span<byte> nullTerminated = stackalloc byte[utf8.Length + 1];

        utf8.CopyTo(nullTerminated);

        nullTerminated[utf8.Length] = 0;

        fixed (byte* pointer = nullTerminated)
        {
            Writeln(pointer);
        }
    }

    public static void Write(ReadOnlySpan<byte> format, ReadOnlySpan<byte> utf8)
    {
        Span<byte> styled = stackalloc byte[format.Length + utf8.Length];

        format.CopyTo(styled);

        utf8.CopyTo(styled.Slice(format.Length));

        Write(styled);
    }

    public static void Write(ReadOnlySpan<byte> utf8)
    {
        Span<byte> nullTerminated = stackalloc byte[utf8.Length + 1];

        utf8.CopyTo(nullTerminated);

        nullTerminated[utf8.Length] = 0;

        fixed (byte* pointer = nullTerminated)
        {
            Write(pointer);
        }
    }

    public static void ReadLine(Span<byte> destination)
    {
        fixed (byte* pointer = destination)
        {
            Readln(pointer, 4096);
        }
    }

    [LibraryImport("nativelib", EntryPoint = "write")]
    private static partial void Write(byte* _string);

    [LibraryImport("nativelib", EntryPoint = "writeln")]
    private static partial void Writeln(byte* _string);

    [LibraryImport("nativelib", EntryPoint = "readln")]
    private static partial void Readln(byte* buffer, int length);
}