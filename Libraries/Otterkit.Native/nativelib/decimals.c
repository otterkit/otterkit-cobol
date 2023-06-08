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

/* Same as the C# type definition */
typedef struct
{
    uint64_t _upperBits;
    uint64_t _lowerBits;
} managedDecQuad;

/* Marshalling helper functions */
decQuad decQuadFromManaged(managedDecQuad value)
{
    decQuad nativeQuad;

    nativeQuad.longs[0] = value._upperBits;
    nativeQuad.longs[1] = value._lowerBits;

    return nativeQuad;
}

managedDecQuad decQuadToManaged(decQuad value)
{
    managedDecQuad managedQuad;

    managedQuad._upperBits = value.longs[0];
    managedQuad._lowerBits = value.longs[1];

    return managedQuad;
}

/* Computational operations */
_export managedDecQuad nativeDecQuadToIntegralValue(managedDecQuad value, enum rounding mode)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    decQuadToIntegralValue(&nativeValue, &nativeValue, &context, mode);

    return decQuadToManaged(nativeValue);
}

_export managedDecQuad nativeDecQuadSqrt(managedDecQuad value)
{
    decQuad nativeValue;
    decNumber decNumValue;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    decQuadToNumber(&nativeValue, &decNumValue);

    decNumberSquareRoot(&decNumValue, &decNumValue, &context);

    decQuadFromNumber(&nativeValue, &decNumValue, &context);

    return decQuadToManaged(nativeValue);
}

_export managedDecQuad nativeDecQuadLn(managedDecQuad value)
{
    decQuad nativeValue;
    decNumber decNumValue;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    decQuadToNumber(&nativeValue, &decNumValue);

    decNumberLn(&decNumValue, &decNumValue, &context);

    decQuadFromNumber(&nativeValue, &decNumValue, &context);

    return decQuadToManaged(nativeValue);
}

_export managedDecQuad nativeDecQuadExp(managedDecQuad value)
{
    decQuad nativeValue;
    decNumber decNumValue;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    decQuadToNumber(&nativeValue, &decNumValue);

    decNumberExp(&decNumValue, &decNumValue, &context);

    decQuadFromNumber(&nativeValue, &decNumValue, &context);

    return decQuadToManaged(nativeValue);
}

_export managedDecQuad nativeDecQuadLogB(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    decQuadLogB(&nativeValue, &nativeValue, &context);

    return decQuadToManaged(nativeValue);
}

_export managedDecQuad nativeDecQuadLog10(managedDecQuad value)
{
    decQuad nativeValue;
    decNumber decNumValue;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    decQuadToNumber(&nativeValue, &decNumValue);

    decNumberLog10(&decNumValue, &decNumValue, &context);

    decQuadFromNumber(&nativeValue, &decNumValue, &context);

    return decQuadToManaged(nativeValue);
}

_export managedDecQuad nativeDecQuadAbs(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    decQuadAbs(&nativeValue, &nativeValue, &context);

    return decQuadToManaged(nativeValue);
}

_export managedDecQuad nativeDecQuadPlus(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    decQuadPlus(&nativeValue, &nativeValue, &context);

    return decQuadToManaged(nativeValue);
}

_export managedDecQuad nativeDecQuadMinus(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    decQuadMinus(&nativeValue, &nativeValue, &context);

    return decQuadToManaged(nativeValue);
}

_export managedDecQuad nativeDecQuadAdd(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadAdd(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadSub(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadSubtract(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadMul(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadMultiply(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadDiv(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadDivide(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadRem(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadRemainder(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadRemNear(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadRemainderNear(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadMax(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadMax(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadMaxMag(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadMaxMag(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadMin(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadMin(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadMinMag(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadMinMag(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadPow(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decNumber decNumLeft;
    decNumber decNumRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadToNumber(&nativeLeft, &decNumLeft);
    decQuadToNumber(&nativeRight, &decNumRight);

    decNumberPower(&decNumLeft, &decNumLeft, &decNumRight, &context);

    decQuadFromNumber(&nativeLeft, &decNumLeft, &context);

    return decQuadToManaged(nativeLeft);
}

_export managedDecQuad nativeDecQuadFMA(managedDecQuad leftMul, managedDecQuad rightMul, managedDecQuad valueAdd)
{
    decQuad nativeLeftMul;
    decQuad nativeRightMul;
    decQuad nativeValueAdd;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeftMul = decQuadFromManaged(leftMul);
    nativeRightMul = decQuadFromManaged(rightMul);
    nativeValueAdd = decQuadFromManaged(valueAdd);

    decQuadFMA(&nativeLeftMul, &nativeLeftMul, &nativeRightMul, &nativeValueAdd, &context);

    return decQuadToManaged(nativeLeftMul);
}


/* decQuad Comparisons */
_export int32_t nativeDecQuadCompare(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadCompare(&nativeLeft, &nativeLeft, &nativeRight, &context);

    if (decQuadIsNaN(&nativeLeft))
        return -5;

    return decQuadToInt32(&nativeLeft, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t nativeDecQuadCompareSignal(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadCompareSignal(&nativeLeft, &nativeLeft, &nativeRight, &context);

    return decQuadToInt32(&nativeLeft, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t nativeDecQuadCompareTotal(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadCompareTotal(&nativeLeft, &nativeLeft, &nativeRight);

    return decQuadToInt32(&nativeLeft, &context, DEC_ROUND_HALF_EVEN);
}

_export int32_t nativeDecQuadCompareTotalMag(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    decQuadCompareTotalMag(&nativeLeft, &nativeLeft, &nativeRight);

    return decQuadToInt32(&nativeLeft, &context, DEC_ROUND_HALF_EVEN);
}

/* Non-computational comparisons */
_export uint32_t nativeDecQuadIsCanonical(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsCanonical(&nativeValue);
}

_export uint32_t nativeDecQuadIsFinite(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsFinite(&nativeValue);
}

_export uint32_t nativeDecQuadIsInfinite(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsInfinite(&nativeValue);
}

_export uint32_t nativeDecQuadIsInteger(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsInteger(&nativeValue);
}

_export uint32_t nativeDecQuadIsNaN(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsNaN(&nativeValue);
}

_export uint32_t nativeDecQuadIsNegative(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsNegative(&nativeValue);
}

_export uint32_t nativeDecQuadIsNormal(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsNormal(&nativeValue);
}

_export uint32_t nativeDecQuadIsSubnormal(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsSubnormal(&nativeValue);
}

_export uint32_t nativeDecQuadIsPositive(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsPositive(&nativeValue);
}

_export uint32_t nativeDecQuadIsSignaling(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsSignaling(&nativeValue);
}

_export uint32_t nativeDecQuadIsSigned(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsSigned(&nativeValue);
}

_export uint32_t nativeDecQuadIsZero(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadIsZero(&nativeValue);
}

_export uint32_t nativeDecQuadRadix(managedDecQuad value)
{
    decQuad nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeValue = decQuadFromManaged(value);

    return decQuadRadix(&nativeValue);
}

_export uint32_t nativeDecQuadSameQuantum(managedDecQuad left, managedDecQuad right)
{
    decQuad nativeLeft;
    decQuad nativeRight;

    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeLeft = decQuadFromManaged(left);
    nativeRight = decQuadFromManaged(right);

    return decQuadSameQuantum(&nativeLeft, &nativeRight);
}

/* Utilities and conversions */
_export char *nativeDecQuadToString(managedDecQuad value)
{
    char *string = malloc(DECQUAD_String);
    decQuad nativeQuad;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeQuad = decQuadFromManaged(value);

    decQuadToString(&nativeQuad, string);

    return string;
}

_export char *nativeDecQuadToEngString(managedDecQuad value)
{
    char *string = malloc(DECQUAD_String);
    decQuad nativeQuad;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    nativeQuad = decQuadFromManaged(value);

    decQuadToEngString(&nativeQuad, string);

    return string;
}

_export managedDecQuad nativeDecQuadFromString(char *value)
{
    decQuad nativeQuad;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECQUAD);

    decQuadFromString(&nativeQuad, value, &context);

    return decQuadToManaged(nativeQuad);
}

_export managedDecQuad nativeDecQuadFromInt32(int32_t value)
{
    decQuad nativeQuad;

    decQuadFromInt32(&nativeQuad, value);

    return decQuadToManaged(nativeQuad);
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
