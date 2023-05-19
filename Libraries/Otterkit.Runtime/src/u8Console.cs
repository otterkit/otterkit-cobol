using System.Buffers;
using System.Text;

namespace Otterkit.Runtime;

public static class u8Console
{
    public static ConsoleColor ForegroundColor
    {
        get => Console.ForegroundColor;

        set => Console.ForegroundColor = value;
    }

    public static ConsoleColor BackgroundColor
    {
        get => Console.BackgroundColor;

        set => Console.BackgroundColor = value;
    }

    public static void WriteLine(ReadOnlySpan<byte> u8String)
    {
        var length = Encoding.UTF8.GetCharCount(u8String);

        Span<char> buffer = stackalloc char[length];

        Encoding.UTF8.GetChars(u8String, buffer);

        Console.Out.WriteLine(buffer);
    }

    public static void Write(ReadOnlySpan<byte> u8String)
    {
        var length = Encoding.UTF8.GetCharCount(u8String);

        Span<char> buffer = stackalloc char[length];

        Encoding.UTF8.GetChars(u8String, buffer);

        Console.Out.Write(buffer);
    }
}
