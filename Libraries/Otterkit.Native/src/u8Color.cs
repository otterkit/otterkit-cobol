namespace Otterkit.Native;

public static class u8Color
{
    public static ReadOnlySpan<byte> Black => "\x1B[30m"u8;
    public static ReadOnlySpan<byte> Red => "\x1B[31m"u8;
    public static ReadOnlySpan<byte> Green => "\x1B[32m"u8;
    public static ReadOnlySpan<byte> Yellow => "\x1B[33m"u8;
    public static ReadOnlySpan<byte> Blue => "\x1B[34m"u8;
    public static ReadOnlySpan<byte> Magenta => "\x1B[35m"u8;
    public static ReadOnlySpan<byte> Cyan => "\x1B[36m"u8;
    public static ReadOnlySpan<byte> White => "\x1B[37m"u8;

    public static ReadOnlySpan<byte> BackgroundBlack => "\x1B[40m"u8;
    public static ReadOnlySpan<byte> BackgroundRed => "\x1B[41m"u8;
    public static ReadOnlySpan<byte> BackgroundGreen => "\x1B[42m"u8;
    public static ReadOnlySpan<byte> BackgroundYellow => "\x1B[43m"u8;
    public static ReadOnlySpan<byte> BackgroundBlue => "\x1B[44m"u8;
    public static ReadOnlySpan<byte> BackgroundMagenta => "\x1B[45m"u8;
    public static ReadOnlySpan<byte> BackgroundCyan => "\x1B[46m"u8;
    public static ReadOnlySpan<byte> BackgroundWhite => "\x1B[47m"u8;

    public static ReadOnlySpan<byte> BrightBlack => "\x1B[90m"u8;
    public static ReadOnlySpan<byte> BrightRed => "\x1B[91m"u8;
    public static ReadOnlySpan<byte> BrightGreen => "\x1B[92m"u8;
    public static ReadOnlySpan<byte> BrightYellow => "\x1B[93m"u8;
    public static ReadOnlySpan<byte> BrightBlue => "\x1B[94m"u8;
    public static ReadOnlySpan<byte> BrightMagenta => "\x1B[95m"u8;
    public static ReadOnlySpan<byte> BrightCyan => "\x1B[96m"u8;
    public static ReadOnlySpan<byte> BrightWhite => "\x1B[97m"u8;

    public static ReadOnlySpan<byte> BrightBackgroundBlack => "\x1B[100m"u8;
    public static ReadOnlySpan<byte> BrightBackgroundRed => "\x1B[101m"u8;
    public static ReadOnlySpan<byte> BrightBackgroundGreen => "\x1B[102m"u8;
    public static ReadOnlySpan<byte> BrightBackgroundYellow => "\x1B[103m"u8;
    public static ReadOnlySpan<byte> BrightBackgroundBlue => "\x1B[104m"u8;
    public static ReadOnlySpan<byte> BrightBackgroundMagenta => "\x1B[105m"u8;
    public static ReadOnlySpan<byte> BrightBackgroundCyan => "\x1B[106m"u8;
    public static ReadOnlySpan<byte> BrightBackgroundWhite => "\x1B[107m"u8;

    public static ReadOnlySpan<byte> Bold => "\x1B[1m"u8;
    public static ReadOnlySpan<byte> Dim => "\x1B[2m"u8;
    public static ReadOnlySpan<byte> Underline => "\x1B[4m"u8;
    public static ReadOnlySpan<byte> Blink => "\x1B[5m"u8;
    public static ReadOnlySpan<byte> Reverse => "\x1B[7m"u8;
    public static ReadOnlySpan<byte> Hide => "\x1B[8m"u8;
}
