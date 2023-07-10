using System.Buffers;
using System.Numerics;
using System.Runtime.CompilerServices;

namespace Otterkit.Runtime;

public static partial class RuntimeHelpers
{
    public static DateTime New_date(Numeric int_fmt)
    {
        DateTime Y1600 = new(1600, 12, 31);
        int days = int.Parse(int_fmt.Display);

        DateTime integer_date = Y1600.AddDays(days);

        return integer_date;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T SetBit<T>(this T value, int position) where T : IBinaryInteger<T>
    {
        return value | (T.One << position);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]

    public static T ClearBit<T>(this T value, int position) where T : IBinaryInteger<T>
    {
        return value & ~(T.One << position);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]

    public static T ToggleBit<T>(this T value, int position) where T : IBinaryInteger<T>
    {
        return value ^ (T.One << position);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]

    public static T FetchBit<T>(this T value, int position) where T : IBinaryInteger<T>
    {
        return (value >> position) & T.One;
    }
}
