using System.Runtime.InteropServices;

namespace Otterkit.Numerics;

internal static partial class DecDoubleBindings
{
    /* Computational operations */
    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleSqrt")]
    internal static partial Decimal64 Sqrt(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleLn")]
    internal static partial Decimal64 Ln(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleExp")]
    internal static partial Decimal64 Exp(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleLogB")]
    internal static partial Decimal64 LogB(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleLog10")]
    internal static partial Decimal64 Log10(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleToIntegralValue")]
    internal static partial Decimal64 ToIntegralValue(Decimal64 value, RoundingMode mode);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleAbs")]
    internal static partial Decimal64 Abs(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoublePlus")]
    internal static partial Decimal64 Plus(Decimal64 value);
    
    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleMinus")]
    internal static partial Decimal64 Minus(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleAdd")]
    internal static partial Decimal64 Add(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleSub")]
    internal static partial Decimal64 Subtract(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleDiv")]
    internal static partial Decimal64 Divide(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleRem")]
    internal static partial Decimal64 Remainder(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleRemNear")]
    internal static partial Decimal64 RemainderNear(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleMul")]
    internal static partial Decimal64 Multiply(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleFMA")]
    internal static partial Decimal64 FusedMultiplyAdd(Decimal64 leftMultiply, Decimal64 rightMultiply, Decimal64 valueAdd);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleMax")]
    internal static partial Decimal64 Max(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleMaxMag")]
    internal static partial Decimal64 MaxMag(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleMin")]
    internal static partial Decimal64 Min(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleMinMag")]
    internal static partial Decimal64 MinMag(Decimal64 left, Decimal64 right);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoublePow")]
    internal static partial Decimal64 Pow(Decimal64 left, Decimal64 right);


    /* decDouble Comparisons */
    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleCompare")]
    internal static partial int Compare(Decimal64 left, Decimal64 right);
    

    /* Non-computational comparisons */
    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsCanonical")]
    internal static partial uint IsCanonical(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsInfinite")]
    internal static partial uint IsInfinite(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsFinite")]
    internal static partial uint IsFinite(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsInteger")]
    internal static partial uint IsInteger(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsNaN")]
    internal static partial uint IsNaN(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsNegative")]
    internal static partial uint IsNegative(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsSubnormal")]
    internal static partial uint IsSubnormal(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsNormal")]
    internal static partial uint IsNormal(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsPositive")]
    internal static partial uint IsPositive(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsSignaling")]
    internal static partial uint IsSignaling(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsSigned")]
    internal static partial uint IsSigned(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleIsZero")]
    internal static partial uint IsZero(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleRadix")]
    internal static partial uint Radix(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecSameQuantum")]
    internal static partial uint SameQuantum(Decimal64 left, Decimal64 right);


    /* Utilities and conversions */
    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleFromString")]
    internal static partial Decimal64 FromString(in byte value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleFromInt32")]
    internal static partial Decimal64 FromInt32(int value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleToString")]
    internal static unsafe partial byte* ToString(Decimal64 value);

    [LibraryImport("decDoubleBindings", EntryPoint = "nativeDecDoubleToEngString")]
    internal static unsafe partial byte* ToEngineeringString(Decimal64 value);
}
