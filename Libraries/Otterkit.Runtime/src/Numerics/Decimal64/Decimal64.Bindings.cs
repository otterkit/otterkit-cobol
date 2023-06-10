using System.Runtime.InteropServices;

namespace Otterkit.Numerics;

internal static partial class Decimal64Bindings
{
    /* Computational operations */
    [LibraryImport("nativelib", EntryPoint = "d64Sqrt")]
    internal static partial Decimal64 Sqrt(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64Ln")]
    internal static partial Decimal64 Ln(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64Exp")]
    internal static partial Decimal64 Exp(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64LogB")]
    internal static partial Decimal64 LogB(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64Log10")]
    internal static partial Decimal64 Log10(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64ToIntegralValue")]
    internal static partial Decimal64 ToIntegralValue(Decimal64 value, RoundingMode mode);

    [LibraryImport("nativelib", EntryPoint = "d64Abs")]
    internal static partial Decimal64 Abs(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64Plus")]
    internal static partial Decimal64 Plus(Decimal64 value);
    
    [LibraryImport("nativelib", EntryPoint = "d64Minus")]
    internal static partial Decimal64 Minus(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64Add")]
    internal static partial Decimal64 Add(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64Sub")]
    internal static partial Decimal64 Subtract(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64Div")]
    internal static partial Decimal64 Divide(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64Rem")]
    internal static partial Decimal64 Remainder(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64RemNear")]
    internal static partial Decimal64 RemainderNear(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64Mul")]
    internal static partial Decimal64 Multiply(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64FMA")]
    internal static partial Decimal64 FusedMultiplyAdd(Decimal64 leftMultiply, Decimal64 rightMultiply, Decimal64 valueAdd);

    [LibraryImport("nativelib", EntryPoint = "d64Max")]
    internal static partial Decimal64 Max(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64MaxMag")]
    internal static partial Decimal64 MaxMag(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64Min")]
    internal static partial Decimal64 Min(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64MinMag")]
    internal static partial Decimal64 MinMag(Decimal64 left, Decimal64 right);

    [LibraryImport("nativelib", EntryPoint = "d64Pow")]
    internal static partial Decimal64 Pow(Decimal64 left, Decimal64 right);


    /* decDouble Comparisons */
    [LibraryImport("nativelib", EntryPoint = "d64Compare")]
    internal static partial int Compare(Decimal64 left, Decimal64 right);
    

    /* Non-computational comparisons */
    [LibraryImport("nativelib", EntryPoint = "d64IsCanonical")]
    internal static partial uint IsCanonical(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsInfinite")]
    internal static partial uint IsInfinite(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsFinite")]
    internal static partial uint IsFinite(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsInteger")]
    internal static partial uint IsInteger(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsNaN")]
    internal static partial uint IsNaN(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsNegative")]
    internal static partial uint IsNegative(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsSubnormal")]
    internal static partial uint IsSubnormal(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsNormal")]
    internal static partial uint IsNormal(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsPositive")]
    internal static partial uint IsPositive(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsSignaling")]
    internal static partial uint IsSignaling(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsSigned")]
    internal static partial uint IsSigned(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64IsZero")]
    internal static partial uint IsZero(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64Radix")]
    internal static partial uint Radix(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64SameQuantum")]
    internal static partial uint SameQuantum(Decimal64 left, Decimal64 right);


    /* Utilities and conversions */
    [LibraryImport("nativelib", EntryPoint = "d64FromString")]
    internal static partial Decimal64 FromString(in byte value);

    [LibraryImport("nativelib", EntryPoint = "d64FromInt32")]
    internal static partial Decimal64 FromInt32(int value);

    [LibraryImport("nativelib", EntryPoint = "d64ToString")]
    internal static unsafe partial byte* ToString(Decimal64 value);

    [LibraryImport("nativelib", EntryPoint = "d64ToEngString")]
    internal static unsafe partial byte* ToEngineeringString(Decimal64 value);
}
