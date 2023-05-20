#include <stdlib.h>

#ifdef _WIN32
    #define _export __declspec(dllexport)
    #define DECUSE64 1

    #include "..\decNumber\decimal128.h"
    #include "..\decNumber\decQuad.h" // decQuad library
#else
    #define _export
    #define DECUSE64 1

    #include "../decNumber/decimal128.h"
    #include "../decNumber/decQuad.h" // decQuad library
#endif

// compile win-x64: cl.exe /O2 /LD decQuadBindings.c ..\decNumber\decContext.c ..\decNumber\decQuad.c ..\decNumber\decNumber.c ..\decNumber\decimal128.c ..\decNumber\decimal64.c

// compile linux-x64: clang -shared -Wl,-rpath -O2 -fPIC -Wall -W -o decQuadBindings.so decQuadBindings.c ../decNumber/decContext.c ../decNumber/decQuad.c ../decNumber/decNumber.c ../decNumber/decimal128.c ../decNumber/decimal64.c

// compile macos-x64: clang -dynamiclib -O2 -fPIC -Wall -W -o decQuadBindings.dylib decQuadBindings.c ../decNumber/decContext.c ../decNumber/decQuad.c ../decNumber/decNumber.c ../decNumber/decimal128.c ../decNumber/decimal64.c

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