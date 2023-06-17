#include <stdlib.h>

#include "common.h"

#define DECUSE64 1

#ifdef _WIN32
    #include "..\decNumber\decimal128.h"
    #include "..\decNumber\decQuad.h" // decQuad library
#else
    #include "../decNumber/decimal128.h"
    #include "../decNumber/decQuad.h" // decQuad library
#endif

/* Computational operations */
_export decQuad d128ToIntegralValue(decQuad value, enum rounding mode)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadToIntegralValue(&result, &value, &context, mode);

    return result;
}

_export decQuad d128Sqrt(decQuad value)
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

_export decQuad d128Ln(decQuad value)
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

_export decQuad d128Exp(decQuad value)
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

_export decQuad d128LogB(decQuad value)
{
    decQuad result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadLogB(&result, &value, &context);

    return result;
}

_export decQuad d128Log10(decQuad value)
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

_export decQuad d128Abs(decQuad value)
{
    decQuad result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadAbs(&result, &value, &context);

    return result;
}

_export decQuad d128Plus(decQuad value)
{
    decQuad result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadPlus(&result, &value, &context);

    return result;
}

_export decQuad d128Minus(decQuad value)
{
    decQuad result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMinus(&result, &value, &context);

    return result;
}

_export decQuad d128Add(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadAdd(&result, &left, &right, &context);

    return result;
}

_export decQuad d128Sub(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadSubtract(&result, &left, &right, &context);

    return result;
}

_export decQuad d128Mul(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMultiply(&result, &left, &right, &context);

    return result;
}

_export decQuad d128Div(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadDivide(&result, &left, &right, &context);

    return result;
}

_export decQuad d128Rem(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadRemainder(&result, &left, &right, &context);

    return result;
}

_export decQuad d128RemNear(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadRemainderNear(&result, &left, &right, &context);

    return result;
}

_export decQuad d128Max(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMax(&result, &left, &right, &context);

    return result;
}

_export decQuad d128MaxMag(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMaxMag(&result, &left, &right, &context);

    return result;
}

_export decQuad d128Min(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMin(&result, &left, &right, &context);

    return result;
}

_export decQuad d128MinMag(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadMinMag(&result, &left, &right, &context);

    return result;
}

_export decQuad d128Pow(decQuad left, decQuad right)
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

_export decQuad d128FMA(decQuad leftMul, decQuad rightMul, decQuad valueAdd)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadFMA(&result, &leftMul, &rightMul, &valueAdd, &context);

    return result;
}


/* decQuad Comparisons */
_export int32_t d128Compare(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadCompare(&result, &left, &right, &context);

    if (decQuadIsNaN(&result)) return -5;

    return decQuadToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t d128CompareSignal(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadCompareSignal(&result, &left, &right, &context);

    return decQuadToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t d128CompareTotal(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadCompareTotal(&result, &left, &right);

    return decQuadToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t d128CompareTotalMag(decQuad left, decQuad right)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadCompareTotalMag(&result, &left, &right);

    return decQuadToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

/* Non-computational comparisons */
_export uint32_t d128IsCanonical(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsCanonical(&value);
}

_export uint32_t d128IsFinite(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsFinite(&value);
}

_export uint32_t d128IsInfinite(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsInfinite(&value);
}

_export uint32_t d128IsInteger(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsInteger(&value);
}

_export uint32_t d128IsNaN(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsNaN(&value);
}

_export uint32_t d128IsNegative(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsNegative(&value);
}

_export uint32_t d128IsNormal(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsNormal(&value);
}

_export uint32_t d128IsSubnormal(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsSubnormal(&value);
}

_export uint32_t d128IsPositive(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsPositive(&value);
}

_export uint32_t d128IsSignaling(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsSignaling(&value);
}

_export uint32_t d128IsSigned(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsSigned(&value);
}

_export uint32_t d128IsZero(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadIsZero(&value);
}

_export uint32_t d128Radix(decQuad value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadRadix(&value);
}

_export uint32_t d128SameQuantum(decQuad left, decQuad right)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    return decQuadSameQuantum(&left, &right);
}

/* Utilities and conversions */
_export char *d128ToString(decQuad value)
{
    char *string = malloc(DECQUAD_String);

    decQuadToString(&value, string);

    return string;
}

_export char *d128ToEngString(decQuad value)
{
    char *string = malloc(DECQUAD_String);

    decQuadToEngString(&value, string);

    return string;
}

_export decQuad d128FromString(char *value)
{
    decContext context;
    decQuad result;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadFromString(&result, value, &context);

    return result;
}

_export decQuad d128FromInt32(int32_t value)
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
_export decDouble d64ToIntegralValue(decDouble value, enum rounding mode)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleToIntegralValue(&result, &value, &context, mode);

    return result;
}

_export decDouble d64Sqrt(decDouble value)
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

_export decDouble d64Ln(decDouble value)
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

_export decDouble d64Exp(decDouble value)
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

_export decDouble d64LogB(decDouble value)
{
    decDouble result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleLogB(&result, &value, &context);

    return result;
}

_export decDouble d64Log10(decDouble value)
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

_export decDouble d64Abs(decDouble value)
{
    decDouble result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleAbs(&result, &value, &context);

    return result;
}

_export decDouble d64Plus(decDouble value)
{
    decDouble result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoublePlus(&result, &value, &context);

    return result;
}

_export decDouble d64Minus(decDouble value)
{
    decDouble result;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMinus(&result, &value, &context);

    return result;
}

_export decDouble d64Add(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleAdd(&result, &left, &right, &context);

    return result;
}

_export decDouble d64Sub(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleSubtract(&result, &left, &right, &context);

    return result;
}

_export decDouble d64Mul(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMultiply(&result, &left, &right, &context);

    return result;
}

_export decDouble d64Div(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleDivide(&result, &left, &right, &context);

    return result;
}

_export decDouble d64Rem(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleRemainder(&result, &left, &right, &context);

    return result;
}

_export decDouble d64RemNear(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleRemainderNear(&result, &left, &right, &context);

    return result;
}

_export decDouble d64Max(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMax(&result, &left, &right, &context);

    return result;
}

_export decDouble d64MaxMag(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMaxMag(&result, &left, &right, &context);

    return result;
}

_export decDouble d64Min(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMin(&result, &left, &right, &context);

    return result;
}

_export decDouble d64MinMag(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleMinMag(&result, &left, &right, &context);

    return result;
}

_export decDouble d64Pow(decDouble left, decDouble right)
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

_export decDouble d64FMA(decDouble leftMul, decDouble rightMul, decDouble valueAdd)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleFMA(&result, &leftMul, &rightMul, &valueAdd, &context);

    return result;
}


/* decDouble Comparisons */
_export int32_t d64Compare(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleCompare(&result, &left, &right, &context);

    if (decDoubleIsNaN(&result)) return -5;

    return decDoubleToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t d64CompareSignal(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleCompareSignal(&result, &left, &right, &context);

    return decDoubleToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t d64CompareTotal(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleCompareTotal(&result, &left, &right);

    return decDoubleToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t d64CompareTotalMag(decDouble left, decDouble right)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleCompareTotalMag(&result, &left, &right);

    return decDoubleToInt32(&result, &context, DEC_ROUND_HALF_EVEN);
}

/* Non-computational comparisons */
_export uint32_t d64IsCanonical(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsCanonical(&value);
}

_export uint32_t d64IsFinite(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsFinite(&value);
}

_export uint32_t d64IsInfinite(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsInfinite(&value);
}

_export uint32_t d64IsInteger(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsInteger(&value);
}

_export uint32_t d64IsNaN(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsNaN(&value);
}

_export uint32_t d64IsNegative(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsNegative(&value);
}

_export uint32_t d64IsNormal(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsNormal(&value);
}

_export uint32_t d64IsSubnormal(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsSubnormal(&value);
}

_export uint32_t d64IsPositive(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsPositive(&value);
}

_export uint32_t d64IsSignaling(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsSignaling(&value);
}

_export uint32_t d64IsSigned(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsSigned(&value);
}

_export uint32_t d64IsZero(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleIsZero(&value);
}

_export uint32_t d64Radix(decDouble value)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleRadix(&value);
}

_export uint32_t d64SameQuantum(decDouble left, decDouble right)
{
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    return decDoubleSameQuantum(&left, &right);
}

/* Utilities and conversions */
_export char *d64ToString(decDouble value)
{
    char *string = malloc(DECDOUBLE_String);

    decDoubleToString(&value, string);

    return string;
}

_export char *d64ToEngString(decDouble value)
{
    char *string = malloc(DECDOUBLE_String);

    decDoubleToEngString(&value, string);

    return string;
}

_export decDouble d64FromString(char *value)
{
    decContext context;
    decDouble result;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleFromString(&result, value, &context);

    return result;
}

_export decDouble d64FromInt32(int32_t value)
{
    decDouble result;

    decDoubleFromInt32(&result, value);

    return result;
}
