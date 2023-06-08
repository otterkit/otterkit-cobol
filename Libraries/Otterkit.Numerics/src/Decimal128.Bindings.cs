using System.Runtime.InteropServices;

namespace Otterkit.Numerics;

internal static partial class DecQuadBindings
{
    /* Computational operations */
    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadSqrt")]
    internal static partial Decimal128 Sqrt(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadLn")]
    internal static partial Decimal128 Ln(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadExp")]
    internal static partial Decimal128 Exp(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadLogB")]
    internal static partial Decimal128 LogB(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadLog10")]
    internal static partial Decimal128 Log10(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadToIntegralValue")]
    internal static partial Decimal128 ToIntegralValue(Decimal128 value, RoundingMode mode);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadAbs")]
    internal static partial Decimal128 Abs(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadPlus")]
    internal static partial Decimal128 Plus(Decimal128 value);
    
    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadMinus")]
    internal static partial Decimal128 Minus(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadAdd")]
    internal static partial Decimal128 Add(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadSub")]
    internal static partial Decimal128 Subtract(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadDiv")]
    internal static partial Decimal128 Divide(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadRem")]
    internal static partial Decimal128 Remainder(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadRemNear")]
    internal static partial Decimal128 RemainderNear(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadMul")]
    internal static partial Decimal128 Multiply(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadFMA")]
    internal static partial Decimal128 FusedMultiplyAdd(Decimal128 leftMultiply, Decimal128 rightMultiply, Decimal128 valueAdd);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadMax")]
    internal static partial Decimal128 Max(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadMaxMag")]
    internal static partial Decimal128 MaxMag(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadMin")]
    internal static partial Decimal128 Min(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadMinMag")]
    internal static partial Decimal128 MinMag(Decimal128 left, Decimal128 right);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadPow")]
    internal static partial Decimal128 Pow(Decimal128 left, Decimal128 right);


    /* decQuad Comparisons */
    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadCompare")]
    internal static partial int Compare(Decimal128 left, Decimal128 right);
    

    /* Non-computational comparisons */
    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsCanonical")]
    internal static partial uint IsCanonical(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsInfinite")]
    internal static partial uint IsInfinite(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsFinite")]
    internal static partial uint IsFinite(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsInteger")]
    internal static partial uint IsInteger(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsNaN")]
    internal static partial uint IsNaN(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsNegative")]
    internal static partial uint IsNegative(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsSubnormal")]
    internal static partial uint IsSubnormal(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsNormal")]
    internal static partial uint IsNormal(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsPositive")]
    internal static partial uint IsPositive(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsSignaling")]
    internal static partial uint IsSignaling(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsSigned")]
    internal static partial uint IsSigned(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadIsZero")]
    internal static partial uint IsZero(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadRadix")]
    internal static partial uint Radix(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecSameQuantum")]
    internal static partial uint SameQuantum(Decimal128 left, Decimal128 right);


    /* Utilities and conversions */
    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadFromString")]
    internal static partial Decimal128 FromString(in byte value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadFromInt32")]
    internal static partial Decimal128 FromInt32(int value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadToString")]
    internal static unsafe partial byte* ToString(Decimal128 value);

    [LibraryImport("nativelib", EntryPoint = "nativeDecQuadToEngString")]
    internal static unsafe partial byte* ToEngineeringString(Decimal128 value);
}
