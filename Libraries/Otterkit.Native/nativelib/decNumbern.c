#include "CASPAL.h"

#define DECUSE64 1
#include "../decNumber/decimal128.h"
#include "../decNumber/decQuad.h" // decQuad library

/* Computational operations */
public decQuad d128ToIntegralValue(decQuad value, enum rounding mode)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadToIntegralValue(&result, &value, &context, mode);

    return result;
}

public decQuad d128Sqrt(decQuad value)
{
    decNumber temporary;
    decQuad result;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadToNumber(&value, &temporary);

    decNumberSquareRoot(&temporary, &temporary, &context);

    decQuadFromNumber(&result, &temporary, &context);

    return result;
}

public decQuad d128Ln(decQuad value)
{
    decNumber temporary;
    decQuad result;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadToNumber(&value, &temporary);

    decNumberLn(&temporary, &temporary, &context);

    decQuadFromNumber(&result, &temporary, &context);

    return result;
}

public decQuad d128Exp(decQuad value)
{
    decNumber temporary;
    decQuad result;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadToNumber(&value, &temporary);

    decNumberExp(&temporary, &temporary, &context);

    decQuadFromNumber(&result, &temporary, &context);

    return result;
}

public decQuad d128LogB(decQuad value)
{
    decQuad result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadLogB(&result, &value, &context);

    return result;
}

public decQuad d128Log10(decQuad value)
{
    decNumber temporary;
    decQuad result;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadToNumber(&value, &temporary);

    decNumberLog10(&temporary, &temporary, &context);

    decQuadFromNumber(&result, &temporary, &context);

    return result;
}

public decQuad d128Abs(decQuad value)
{
    decQuad result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadAbs(&result, &value, &context);

    return result;
}

public decQuad d128Plus(decQuad value)
{
    decQuad result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadPlus(&result, &value, &context);

    return result;
}

public decQuad d128Minus(decQuad value)
{
    decQuad result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMinus(&result, &value, &context);

    return result;
}

public decQuad d128Add(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadAdd(&result, &left, &right, &context);

    return result;
}

public decQuad d128Sub(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadSubtract(&result, &left, &right, &context);

    return result;
}

public decQuad d128Mul(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMultiply(&result, &left, &right, &context);

    return result;
}

public decQuad d128Div(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadDivide(&result, &left, &right, &context);

    return result;
}

public decQuad d128Rem(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadRemainder(&result, &left, &right, &context);

    return result;
}

public decQuad d128RemNear(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadRemainderNear(&result, &left, &right, &context);

    return result;
}

public decQuad d128Max(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMax(&result, &left, &right, &context);

    return result;
}

public decQuad d128MaxMag(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMaxMag(&result, &left, &right, &context);

    return result;
}

public decQuad d128Min(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMin(&result, &left, &right, &context);

    return result;
}

public decQuad d128MinMag(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMinMag(&result, &left, &right, &context);

    return result;
}

public decQuad d128Pow(decQuad left, decQuad right)
{
    decQuad result;

    decNumber tempLeft;
    decNumber tempRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadToNumber(&left, &tempLeft);
    decQuadToNumber(&right, &tempRight);

    decNumberPower(&tempLeft, &tempLeft, &tempRight, &context);

    decQuadFromNumber(&result, &tempLeft, &context);

    return result;
}

public decQuad d128FMA(decQuad leftMul, decQuad rightMul, decQuad valueAdd)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadFMA(&result, &leftMul, &rightMul, &valueAdd, &context);

    return result;
}


/* decQuad Comparisons */
public int32 d128Compare(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadCompare(&result, &left, &right, &context);

    if (decQuadIsNaN(&result)) return -5;

    return decQuadToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

public int32 d128CompareSignal(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadCompareSignal(&result, &left, &right, &context);

    return decQuadToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

public int32 d128CompareTotal(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadCompareTotal(&result, &left, &right);

    return decQuadToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

public int32 d128CompareTotalMag(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadCompareTotalMag(&result, &left, &right);

    return decQuadToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

/* Non-computational comparisons */
public uint32 d128IsCanonical(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsCanonical(&value);
}

public uint32 d128IsFinite(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsFinite(&value);
}

public uint32 d128IsInfinite(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsInfinite(&value);
}

public uint32 d128IsInteger(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsInteger(&value);
}

public uint32 d128IsNaN(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsNaN(&value);
}

public uint32 d128IsNegative(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsNegative(&value);
}

public uint32 d128IsNormal(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsNormal(&value);
}

public uint32 d128IsSubnormal(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsSubnormal(&value);
}

public uint32 d128IsPositive(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsPositive(&value);
}

public uint32 d128IsSignaling(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsSignaling(&value);
}

public uint32 d128IsSigned(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsSigned(&value);
}

public uint32 d128IsZero(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsZero(&value);
}

public uint32 d128Radix(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadRadix(&value);
}

public uint32 d128SameQuantum(decQuad left, decQuad right)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadSameQuantum(&left, &right);
}

/* Utilities and conversions */
public int32 d128ToPacked(decQuad value, int32 *exponent, uint8 *packed)
{
    return decQuadToPacked(&value, exponent, packed);
}

public decQuad d128FromPacked(int32 exponent, const uint8 *packed)
{
    decQuad result;

    decQuadFromPacked(&result, exponent, packed);

    return result;
}

public char *d128ToString(decQuad value)
{
    char *string = malloc(DECQUAD_String);

    decQuadToString(&value, string);

    return string;
}

public char *d128ToEngString(decQuad value)
{
    char *string = malloc(DECQUAD_String);

    decQuadToEngString(&value, string);

    return string;
}

public decQuad d128FromString(char *value)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadFromString(&result, value, &context);

    return result;
}

public decQuad d128FromInt32(int32 value)
{
    decQuad result;

    decQuadFromInt32(&result, value);

    return result;
}

#ifdef _WIN32
    #include "..\decNumber\decimal64.h"
    #include "..\decNumber\decDouble.h" // decDouble library
#else
    #include "../decNumber/decimal64.h"
    #include "../decNumber/decDouble.h" // decDouble library
#endif

/* Computational operations */
public decDouble d64ToIntegralValue(decDouble value, enum rounding mode)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleToIntegralValue(&result, &value, &context, mode);

    return result;
}

public decDouble d64Sqrt(decDouble value)
{
    decNumber temporary;
    decDouble result;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleToNumber(&value, &temporary);

    decNumberSquareRoot(&temporary, &temporary, &context);

    decDoubleFromNumber(&result, &temporary, &context);

    return result;
}

public decDouble d64Ln(decDouble value)
{
    decNumber temporary;
    decDouble result;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleToNumber(&value, &temporary);

    decNumberLn(&temporary, &temporary, &context);

    decDoubleFromNumber(&result, &temporary, &context);

    return result;
}

public decDouble d64Exp(decDouble value)
{
    decNumber temporary;
    decDouble result;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleToNumber(&value, &temporary);

    decNumberExp(&temporary, &temporary, &context);

    decDoubleFromNumber(&result, &temporary, &context);

    return result;
}

public decDouble d64LogB(decDouble value)
{
    decDouble result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleLogB(&result, &value, &context);

    return result;
}

public decDouble d64Log10(decDouble value)
{
    decNumber temporary;
    decDouble result;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleToNumber(&value, &temporary);

    decNumberLog10(&temporary, &temporary, &context);

    decDoubleFromNumber(&result, &temporary, &context);

    return result;
}

public decDouble d64Abs(decDouble value)
{
    decDouble result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleAbs(&result, &value, &context);

    return result;
}

public decDouble d64Plus(decDouble value)
{
    decDouble result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoublePlus(&result, &value, &context);

    return result;
}

public decDouble d64Minus(decDouble value)
{
    decDouble result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMinus(&result, &value, &context);

    return result;
}

public decDouble d64Add(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleAdd(&result, &left, &right, &context);

    return result;
}

public decDouble d64Sub(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleSubtract(&result, &left, &right, &context);

    return result;
}

public decDouble d64Mul(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMultiply(&result, &left, &right, &context);

    return result;
}

public decDouble d64Div(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleDivide(&result, &left, &right, &context);

    return result;
}

public decDouble d64Rem(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleRemainder(&result, &left, &right, &context);

    return result;
}

public decDouble d64RemNear(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleRemainderNear(&result, &left, &right, &context);

    return result;
}

public decDouble d64Max(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMax(&result, &left, &right, &context);

    return result;
}

public decDouble d64MaxMag(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMaxMag(&result, &left, &right, &context);

    return result;
}

public decDouble d64Min(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMin(&result, &left, &right, &context);

    return result;
}

public decDouble d64MinMag(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMinMag(&result, &left, &right, &context);

    return result;
}

public decDouble d64Pow(decDouble left, decDouble right)
{
    decDouble result;

    decNumber tempLeft;
    decNumber tempRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleToNumber(&left, &tempLeft);
    decDoubleToNumber(&right, &tempRight);

    decNumberPower(&tempLeft, &tempLeft, &tempRight, &context);

    decDoubleFromNumber(&result, &tempLeft, &context);

    return result;
}

public decDouble d64FMA(decDouble leftMul, decDouble rightMul, decDouble valueAdd)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleFMA(&result, &leftMul, &rightMul, &valueAdd, &context);

    return result;
}


/* decDouble Comparisons */
public int32 d64Compare(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleCompare(&result, &left, &right, &context);

    if (decDoubleIsNaN(&result)) return -5;

    return decDoubleToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

public int32 d64CompareSignal(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleCompareSignal(&result, &left, &right, &context);

    return decDoubleToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

public int32 d64CompareTotal(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleCompareTotal(&result, &left, &right);

    return decDoubleToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

public int32 d64CompareTotalMag(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleCompareTotalMag(&result, &left, &right);

    return decDoubleToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

/* Non-computational comparisons */
public uint32 d64IsCanonical(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsCanonical(&value);
}

public uint32 d64IsFinite(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsFinite(&value);
}

public uint32 d64IsInfinite(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsInfinite(&value);
}

public uint32 d64IsInteger(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsInteger(&value);
}

public uint32 d64IsNaN(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsNaN(&value);
}

public uint32 d64IsNegative(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsNegative(&value);
}

public uint32 d64IsNormal(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsNormal(&value);
}

public uint32 d64IsSubnormal(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsSubnormal(&value);
}

public uint32 d64IsPositive(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsPositive(&value);
}

public uint32 d64IsSignaling(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsSignaling(&value);
}

public uint32 d64IsSigned(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsSigned(&value);
}

public uint32 d64IsZero(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsZero(&value);
}

public uint32 d64Radix(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleRadix(&value);
}

public uint32 d64SameQuantum(decDouble left, decDouble right)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleSameQuantum(&left, &right);
}

/* Utilities and conversions */
public char *d64ToString(decDouble value)
{
    char *string = malloc(DECDOUBLE_String);

    decDoubleToString(&value, string);

    return string;
}

public char *d64ToEngString(decDouble value)
{
    char *string = malloc(DECDOUBLE_String);

    decDoubleToEngString(&value, string);

    return string;
}

public decDouble d64FromString(char *value)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleFromString(&result, value, &context);

    return result;
}

public decDouble d64FromInt32(int32 value)
{
    decDouble result;

    decDoubleFromInt32(&result, value);

    return result;
}
