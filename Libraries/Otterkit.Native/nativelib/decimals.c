#include <stdlib.h>

#define DECUSE64 1

#ifdef _WIN32
    #define _export __declspec(dllexport)

    #include "..\decNumber\decimal128.h"
    #include "..\decNumber\decQuad.h" // decQuad library
#else
    #define _export

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

    decQuadToNumber(&result, &temporary);

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

    decQuadToNumber(&result, &temporary);

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

    decQuadToNumber(&result, &temporary);

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

    decQuadToNumber(&result, &temporary);

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
    #define _export __declspec(dllexport)

    #include "..\decNumber\decimal64.h"
    #include "..\decNumber\decDouble.h" // decDouble library
#else
    #define _export

    #include "../decNumber/decimal64.h"
    #include "../decNumber/decDouble.h" // decDouble library
#endif

/* Same as the C# type definition */
typedef struct
{
    uint64_t _Bits;
} managedDecDouble;

/* Marshalling helper functions */
decDouble decDoubleFromManaged(managedDecDouble value)
{
    decDouble nativeDouble;

    nativeDouble.longs[0] = value._Bits;

    return nativeDouble;
}

managedDecDouble decDoubleToManaged(decDouble value)
{
    managedDecDouble managedDouble;

    managedDouble._Bits = value.longs[0];

    return managedDouble;
}

/* Computational operations */
_export managedDecDouble nativeDecDoubleToIntegralValue(managedDecDouble value, enum rounding mode)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleToIntegralValue(&nativeValue, &nativeValue, &context, mode);

    return decDoubleToManaged(nativeValue);
}

_export managedDecDouble nativeDecDoubleSqrt(managedDecDouble value)
{
    decDouble nativeValue;
    decNumber decNumValue;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleToNumber(&nativeValue, &decNumValue);

    decNumberSquareRoot(&decNumValue, &decNumValue, &context);

    decDoubleFromNumber(&nativeValue, &decNumValue, &context);

    return decDoubleToManaged(nativeValue);
}

_export managedDecDouble nativeDecDoubleLn(managedDecDouble value)
{
    decDouble nativeValue;
    decNumber decNumValue;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleToNumber(&nativeValue, &decNumValue);

    decNumberLn(&decNumValue, &decNumValue, &context);

    decDoubleFromNumber(&nativeValue, &decNumValue, &context);

    return decDoubleToManaged(nativeValue);
}

_export managedDecDouble nativeDecDoubleExp(managedDecDouble value)
{
    decDouble nativeValue;
    decNumber decNumValue;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleToNumber(&nativeValue, &decNumValue);

    decNumberExp(&decNumValue, &decNumValue, &context);

    decDoubleFromNumber(&nativeValue, &decNumValue, &context);

    return decDoubleToManaged(nativeValue);
}

_export managedDecDouble nativeDecDoubleLogB(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleLogB(&nativeValue, &nativeValue, &context);

    return decDoubleToManaged(nativeValue);
}

_export managedDecDouble nativeDecDoubleLog10(managedDecDouble value)
{
    decDouble nativeValue;
    decNumber decNumValue;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleToNumber(&nativeValue, &decNumValue);

    decNumberLog10(&decNumValue, &decNumValue, &context);

    decDoubleFromNumber(&nativeValue, &decNumValue, &context);

    return decDoubleToManaged(nativeValue);
}

_export managedDecDouble nativeDecDoubleAbs(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleAbs(&nativeValue, &nativeValue, &context);

    return decDoubleToManaged(nativeValue);
}

_export managedDecDouble nativeDecDoublePlus(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoublePlus(&nativeValue, &nativeValue, &context);

    return decDoubleToManaged(nativeValue);
}

_export managedDecDouble nativeDecDoubleMinus(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleMinus(&nativeValue, &nativeValue, &context);

    return decDoubleToManaged(nativeValue);
}

_export managedDecDouble nativeDecDoubleAdd(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleAdd(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleSub(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleSubtract(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleMul(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleMultiply(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleDiv(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleDivide(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleRem(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleRemainder(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleRemNear(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleRemainderNear(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleMax(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleMax(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleMaxMag(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleMaxMag(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleMin(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleMin(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleMinMag(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleMinMag(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoublePow(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decNumber decNumLeft;
    decNumber decNumRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleToNumber(&nativeLeft, &decNumLeft);
    decDoubleToNumber(&nativeRight, &decNumRight);

    decNumberPower(&decNumLeft, &decNumLeft, &decNumRight, &context);

    decDoubleFromNumber(&nativeLeft, &decNumLeft, &context);

    return decDoubleToManaged(nativeLeft);
}

_export managedDecDouble nativeDecDoubleFMA(managedDecDouble leftMul, managedDecDouble rightMul, managedDecDouble valueAdd)
{
    decDouble nativeLeftMul;
    decDouble nativeRightMul;
    decDouble nativeValueAdd;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeftMul = decDoubleFromManaged(leftMul);
    nativeRightMul = decDoubleFromManaged(rightMul);
    nativeValueAdd = decDoubleFromManaged(valueAdd);

    decDoubleFMA(&nativeLeftMul, &nativeLeftMul, &nativeRightMul, &nativeValueAdd, &context);

    return decDoubleToManaged(nativeLeftMul);
}


/* decDouble Comparisons */
_export int32_t nativeDecDoubleCompare(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleCompare(&nativeLeft, &nativeLeft, &nativeRight, &context);

    if (decDoubleIsNaN(&nativeLeft))
        return -5;

    return decDoubleToInt32(&nativeLeft, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t nativeDecDoubleCompareSignal(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleCompareSignal(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decDoubleToInt32(&nativeLeft, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t nativeDecDoubleCompareTotal(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleCompareTotal(&nativeLeft, &nativeLeft, &nativeRight);

    return decDoubleToInt32(&nativeLeft, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t nativeDecDoubleCompareTotalMag(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    decDoubleCompareTotalMag(&nativeLeft, &nativeLeft, &nativeRight);

    return decDoubleToInt32(&nativeLeft, &context, DEC_ROUND_HALF_EVEN);
}

/* Non-computational comparisons */
_export uint32_t nativeDecDoubleIsCanonical(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsCanonical(&nativeValue);
}

_export uint32_t nativeDecDoubleIsFinite(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsFinite(&nativeValue);
}

_export uint32_t nativeDecDoubleIsInfinite(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsInfinite(&nativeValue);
}

_export uint32_t nativeDecDoubleIsInteger(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsInteger(&nativeValue);
}

_export uint32_t nativeDecDoubleIsNaN(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsNaN(&nativeValue);
}

_export uint32_t nativeDecDoubleIsNegative(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsNegative(&nativeValue);
}

_export uint32_t nativeDecDoubleIsNormal(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsNormal(&nativeValue);
}

_export uint32_t nativeDecDoubleIsSubnormal(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsSubnormal(&nativeValue);
}

_export uint32_t nativeDecDoubleIsPositive(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsPositive(&nativeValue);
}

_export uint32_t nativeDecDoubleIsSignaling(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsSignaling(&nativeValue);
}

_export uint32_t nativeDecDoubleIsSigned(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsSigned(&nativeValue);
}

_export uint32_t nativeDecDoubleIsZero(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsZero(&nativeValue);
}

_export uint32_t nativeDecDoubleRadix(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleRadix(&nativeValue);
}

_export uint32_t nativeDecDoubleSameQuantum(managedDecDouble left, managedDecDouble right)
{
    decDouble nativeLeft;
    decDouble nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeLeft = decDoubleFromManaged(left);
    nativeRight = decDoubleFromManaged(right);

    return decDoubleSameQuantum(&nativeLeft, &nativeRight);
}

/* Utilities and conversions */
_export char *nativeDecDoubleToString(managedDecDouble value)
{
    char *string = malloc(DECDOUBLE_String);
    decDouble nativeDouble;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeDouble = decDoubleFromManaged(value);

    decDoubleToString(&nativeDouble, string);

    return string;
}

_export char *nativeDecDoubleToEngString(managedDecDouble value)
{
    char *string = malloc(DECDOUBLE_String);
    decDouble nativeDouble;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeDouble = decDoubleFromManaged(value);

    decDoubleToEngString(&nativeDouble, string);

    return string;
}

_export managedDecDouble nativeDecDoubleFromString(char *value)
{
    decDouble nativeDouble;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleFromString(&nativeDouble, value, &context);

    return decDoubleToManaged(nativeDouble);
}

_export managedDecDouble nativeDecDoubleFromInt32(int32_t value)
{
    decDouble nativeDouble;

    decDoubleFromInt32(&nativeDouble, value);

    return decDoubleToManaged(nativeDouble);
}
