using System.Runtime.InteropServices;

namespace Otterkit.Numerics;

internal static partial class Decimal128Bindings
{
    /* Computational operations */
    [LibraryImport("nativelib", EntryPoint = "d128Sqrt")]
    internal static partial Decimal128 Sqrt(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128Ln")]
    internal static partial Decimal128 Ln(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128Exp")]
    internal static partial Decimal128 Exp(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128LogB")]
    internal static partial Decimal128 LogB(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128Log10")]
    internal static partial Decimal128 Log10(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128ToIntegralValue")]
    internal static partial Decimal128 ToIntegralValue(Decimal128 value, RoundingMode mode);

    [LibraryImport("nativelib", EntryPoint = "d128Abs")]
    internal static partial Decimal128 Abs(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128Plus")]
    internal static partial Decimal128 Plus(Decimal128 value);
    
    [LibraryImport("nativelib", EntryPoint = "d128Minus")]
    internal static partial Decimal128 Minus(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128Add")]
    internal static partial Decimal128 Add(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128Sub")]
    internal static partial Decimal128 Subtract(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128Div")]
    internal static partial Decimal128 Divide(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128Rem")]
    internal static partial Decimal128 Remainder(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128RemNear")]
    internal static partial Decimal128 RemainderNear(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128Mul")]
    internal static partial Decimal128 Multiply(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128FMA")]
    internal static partial Decimal128 FusedMultiplyAdd(Decimal128 leftMultiply, Decimal128 rightMultiply, Decimal128 valueAdd);

    [LibraryImport("nativelib", EntryPoint = "d128Max")]
    internal static partial Decimal128 Max(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128MaxMag")]
    internal static partial Decimal128 MaxMag(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128Min")]
    internal static partial Decimal128 Min(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128MinMag")]
    internal static partial Decimal128 MinMag(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "d128Pow")]
    internal static partial Decimal128 Pow(Decimal128 left, Decimal128 right);


    /* decQuad Comparisons */
    [LibraryImport("nativelib", EntryPoint = "d128Compare")]
    internal static partial int Compare(Decimal128 left, Decimal128 right);
    

    /* Non-computational comparisons */
    [LibraryImport("nativelib", EntryPoint = "d128IsCanonical")]
    internal static partial uint IsCanonical(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsInfinite")]
    internal static partial uint IsInfinite(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsFinite")]
    internal static partial uint IsFinite(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsInteger")]
    internal static partial uint IsInteger(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsNaN")]
    internal static partial uint IsNaN(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsNegative")]
    internal static partial uint IsNegative(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsSubnormal")]
    internal static partial uint IsSubnormal(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsNormal")]
    internal static partial uint IsNormal(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsPositive")]
    internal static partial uint IsPositive(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsSignaling")]
    internal static partial uint IsSignaling(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsSigned")]
    internal static partial uint IsSigned(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128IsZero")]
    internal static partial uint IsZero(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128Radix")]
    internal static partial uint Radix(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128SameQuantum")]
    internal static partial uint SameQuantum(Decimal128 left, Decimal128 right);


    /* Utilities and conversions */
    [LibraryImport("nativelib", EntryPoint = "d128ToPacked")]
    internal static partial int ToPacked(Decimal128 value, ref int exponent, ref byte buffer);

    [LibraryImport("nativelib", EntryPoint = "d128FromPacked")]
    internal static partial Decimal128 FromPacked(int exponent, in byte buffer);

    [LibraryImport("nativelib", EntryPoint = "d128FromString")]
    internal static partial Decimal128 FromString(in byte value);

    [LibraryImport("nativelib", EntryPoint = "d128FromInt32")]
    internal static partial Decimal128 FromInt32(int value);

    [LibraryImport("nativelib", EntryPoint = "d128ToString")]
    internal static unsafe partial byte* ToString(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "d128ToEngString")]
    internal static unsafe partial byte* ToEngineeringString(Decimal128 value);
}
