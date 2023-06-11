using System.Buffers;

namespace Otterkit.Runtime;

public static partial class RuntimeHelpers
{
    private static ArrayPool<byte> Pool = ArrayPool<byte>.Shared;

    public static DateTime New_date(Numeric int_fmt)
    {
        DateTime Y1600 = new(1600, 12, 31);
        int days = int.Parse(int_fmt.Display);

        DateTime integer_date = Y1600.AddDays(days);

        return integer_date;
    }

    public static bool EqualsFold(ReadOnlySpan<byte> u8String, ReadOnlySpan<byte> u8Other)
    {
        var fold = u8String.CaseFold();
        var otherFold = u8Other.CaseFold();

        var equals = fold.SequenceEqual(otherFold);

        Pool.Return(fold);
        Pool.Return(otherFold);

        return equals;
    }

    public static byte[] CaseFold(this ReadOnlySpan<byte> u8String)
    {
        var length = u8String.Length;

        var index = 0;

        var destinationIndex = 0;

        var destination = Pool.Rent(length);

        while (index < length)
        {
            var codepointFold = u8String
                .AsFoldable(index)
                .FoldCase();

            codepointFold.CopyTo(destination.AsSpan(destinationIndex));

            index += codepointFold.Length;

            destinationIndex += codepointFold.Length;
        }

        return destination;
    }

    private static ReadOnlySpan<byte> FoldCase(this ReadOnlySpan<byte> u8String)
    {
        var foldLength = CaseFoldLength(u8String[0]);

        if (foldLength is 0) return u8String;

        var mapping = CaseFoldLookup(u8String);

        return CaseFoldData.AsSpan(mapping, foldLength);
    }

    private static ReadOnlySpan<byte> AsFoldable(this ReadOnlySpan<byte> u8String, int index)
    {
        var foldLength = CaseFoldLength(u8String[index]);

        if (foldLength is 0) return u8String.Slice(index, 1);

        return u8String.Slice(index, foldLength);
    }

    private static int CaseFoldLength(byte _byte)
    {
        return _byte switch
        {
            // 1 byte
            > 64 and < 91 => 1,
            // 2 bytes
            > 193 and < 202 or > 204 and < 214 => 2,
            // 3 bytes
            225 or 226 or 234 or 239 => 3,
            // 4 bytes
            240 => 4,
            // Not a possible case fold
            _ => 0
        };
    }
}
