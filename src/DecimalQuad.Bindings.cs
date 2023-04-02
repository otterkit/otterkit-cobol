using System.Runtime.InteropServices;

namespace Otterkit.Numerics;

internal static partial class DecQuadBindings
{
    /* Computational operations */
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadAbs")]
    internal static partial DecimalQuad Abs(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadPlus")]
    internal static partial DecimalQuad Plus(DecimalQuad value);
    
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadMinus")]
    internal static partial DecimalQuad Minus(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadAdd")]
    internal static partial DecimalQuad Add(DecimalQuad left, DecimalQuad right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadSub")]
    internal static partial DecimalQuad Subtract(DecimalQuad left, DecimalQuad right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadDiv")]
    internal static partial DecimalQuad Divide(DecimalQuad left, DecimalQuad right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadRem")]
    internal static partial DecimalQuad Remainder(DecimalQuad left, DecimalQuad right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadRemNear")]
    internal static partial DecimalQuad RemainderNear(DecimalQuad left, DecimalQuad right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadMul")]
    internal static partial DecimalQuad Multiply(DecimalQuad left, DecimalQuad right);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadFMA")]
    internal static partial DecimalQuad FusedMultiplyAdd(DecimalQuad leftMultiply, DecimalQuad rightMultiply, DecimalQuad valueAdd);


    /* decQuad Comparisons */


    /* Non-computational comparisons */
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsCanonical")]
    internal static partial uint IsCanonical(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsFinite")]
    internal static partial uint IsFinite(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsInteger")]
    internal static partial uint IsInteger(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsNaN")]
    internal static partial uint IsNaN(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsNegative")]
    internal static partial uint IsNegative(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsNormal")]
    internal static partial uint IsNormal(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsPositive")]
    internal static partial uint IsPositive(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsSignaling")]
    internal static partial uint IsSignaling(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsSigned")]
    internal static partial uint IsSigned(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadIsZero")]
    internal static partial uint IsZero(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadRadix")]
    internal static partial uint Radix(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecSameQuantum")]
    internal static partial uint SameQuantum(DecimalQuad left, DecimalQuad right);


    /* Utilities and conversions */
    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadFromString")]
    internal static partial DecimalQuad FromString(in byte value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadToString")]
    internal static unsafe partial byte* ToString(DecimalQuad value);

    [LibraryImport("decQuadBindings", EntryPoint = "nativeDecQuadToEngString")]
    internal static unsafe partial byte* ToEngineeringString(DecimalQuad value);
}
