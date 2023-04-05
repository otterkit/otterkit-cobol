using System.Runtime.InteropServices;

namespace Otterkit.Numerics;

internal static partial class DecQuadBindings
{
    /* Computational operations */
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadSqrt")]
    internal static partial Decimal128 Sqrt(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadLn")]
    internal static partial Decimal128 Ln(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadExp")]
    internal static partial Decimal128 Exp(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadAbs")]
    internal static partial Decimal128 Abs(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadPlus")]
    internal static partial Decimal128 Plus(Decimal128 value);
    
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadMinus")]
    internal static partial Decimal128 Minus(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadAdd")]
    internal static partial Decimal128 Add(Decimal128 left, Decimal128 right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadSub")]
    internal static partial Decimal128 Subtract(Decimal128 left, Decimal128 right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadDiv")]
    internal static partial Decimal128 Divide(Decimal128 left, Decimal128 right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadRem")]
    internal static partial Decimal128 Remainder(Decimal128 left, Decimal128 right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadRemNear")]
    internal static partial Decimal128 RemainderNear(Decimal128 left, Decimal128 right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadMul")]
    internal static partial Decimal128 Multiply(Decimal128 left, Decimal128 right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadFMA")]
    internal static partial Decimal128 FusedMultiplyAdd(Decimal128 leftMultiply, Decimal128 rightMultiply, Decimal128 valueAdd);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadMax")]
    internal static partial Decimal128 Max(Decimal128 left, Decimal128 right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadMin")]
    internal static partial Decimal128 Min(Decimal128 left, Decimal128 right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadPow")]
    internal static partial Decimal128 Pow(Decimal128 left, Decimal128 right);


    /* decQuad Comparisons */
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadCompare")]
    internal static partial int Compare(Decimal128 left, Decimal128 right);
    

    /* Non-computational comparisons */
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsCanonical")]
    internal static partial uint IsCanonical(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsFinite")]
    internal static partial uint IsFinite(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsInteger")]
    internal static partial uint IsInteger(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsNaN")]
    internal static partial uint IsNaN(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsNegative")]
    internal static partial uint IsNegative(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsNormal")]
    internal static partial uint IsNormal(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsPositive")]
    internal static partial uint IsPositive(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsSignaling")]
    internal static partial uint IsSignaling(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsSigned")]
    internal static partial uint IsSigned(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsZero")]
    internal static partial uint IsZero(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadRadix")]
    internal static partial uint Radix(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecSameQuantum")]
    internal static partial uint SameQuantum(Decimal128 left, Decimal128 right);


    /* Utilities and conversions */
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadFromString")]
    internal static partial Decimal128 FromString(in byte value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadFromInt32")]
    internal static partial Decimal128 FromInt32(int value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadToString")]
    internal static unsafe partial byte* ToString(Decimal128 value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadToEngString")]
    internal static unsafe partial byte* ToEngineeringString(Decimal128 value);
}
