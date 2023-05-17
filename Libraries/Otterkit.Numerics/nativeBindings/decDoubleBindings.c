#include <stdlib.h>

#ifdef _WIN32
    #define DLLEXPORT __declspec(dllexport)
    #define DECUSE64 1

    #include "..\decNumber\decimal64.h"
    #include "..\decNumber\decDouble.h" // decDouble library
#endif

#ifndef _WIN32
    #define DLLEXPORT
    #define DECUSE64 1

    #include "../decNumber/decimal64.h"
    #include "../decNumber/decDouble.h" // decDouble library
#endif

// compile win-x64: cl.exe /O2 /LD decDoubleBindings.c ..\decNumber\decContext.c ..\decNumber\decQuad.c ..\decNumber\decDouble.c ..\decNumber\decNumber.c ..\decNumber\decimal128.c ..\decNumber\decimal64.c

// compile linux-x64: clang -shared -Wl,-rpath -O2 -fPIC -Wall -W -o decDoubleBindings.so decDoubleBindings.c ../decNumber/decContext.c ../decNumber/decQuad.c ../decNumber/decDouble.c ../decNumber/decNumber.c ../decNumber/decimal128.c ../decNumber/decimal64.c

// compile macos-x64: clang -dynamiclib -O2 -fPIC -Wall -W -o decDoubleBindings.dylib decDoubleBindings.c ../decNumber/decContext.c ../decNumber/decQuad.c ../decNumber/decDouble.c ../decNumber/decNumber.c ../decNumber/decimal128.c ../decNumber/decimal64.c

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
DLLEXPORT
managedDecDouble nativeDecDoubleToIntegralValue(managedDecDouble value, enum rounding mode)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleToIntegralValue(&nativeValue, &nativeValue, &context, mode);

    return decDoubleToManaged(nativeValue);
}

DLLEXPORT
managedDecDouble nativeDecDoubleSqrt(managedDecDouble value)
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

DLLEXPORT
managedDecDouble nativeDecDoubleLn(managedDecDouble value)
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

DLLEXPORT
managedDecDouble nativeDecDoubleExp(managedDecDouble value)
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

DLLEXPORT
managedDecDouble nativeDecDoubleLogB(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleLogB(&nativeValue, &nativeValue, &context);

    return decDoubleToManaged(nativeValue);
}

DLLEXPORT
managedDecDouble nativeDecDoubleLog10(managedDecDouble value)
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

DLLEXPORT
managedDecDouble nativeDecDoubleAbs(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleAbs(&nativeValue, &nativeValue, &context);

    return decDoubleToManaged(nativeValue);
}

DLLEXPORT
managedDecDouble nativeDecDoublePlus(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoublePlus(&nativeValue, &nativeValue, &context);

    return decDoubleToManaged(nativeValue);
}

DLLEXPORT
managedDecDouble nativeDecDoubleMinus(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    decDoubleMinus(&nativeValue, &nativeValue, &context);

    return decDoubleToManaged(nativeValue);
}

DLLEXPORT
managedDecDouble nativeDecDoubleAdd(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleSub(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleMul(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleDiv(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleRem(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleRemNear(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleMax(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleMaxMag(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleMin(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleMinMag(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoublePow(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
managedDecDouble nativeDecDoubleFMA(managedDecDouble leftMul, managedDecDouble rightMul, managedDecDouble valueAdd)
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
DLLEXPORT
int32_t nativeDecDoubleCompare(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
int32_t nativeDecDoubleCompareSignal(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
int32_t nativeDecDoubleCompareTotal(managedDecDouble left, managedDecDouble right)
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

DLLEXPORT
int32_t nativeDecDoubleCompareTotalMag(managedDecDouble left, managedDecDouble right)
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
DLLEXPORT
uint32_t nativeDecDoubleIsCanonical(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsCanonical(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsFinite(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsFinite(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsInfinite(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsInfinite(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsInteger(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsInteger(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsNaN(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsNaN(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsNegative(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsNegative(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsNormal(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsNormal(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsSubnormal(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsSubnormal(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsPositive(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsPositive(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsSignaling(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsSignaling(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsSigned(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsSigned(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleIsZero(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleIsZero(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleRadix(managedDecDouble value)
{
    decDouble nativeValue;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeValue = decDoubleFromManaged(value);

    return decDoubleRadix(&nativeValue);
}

DLLEXPORT
uint32_t nativeDecDoubleSameQuantum(managedDecDouble left, managedDecDouble right)
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
DLLEXPORT
char *nativeDecDoubleToString(managedDecDouble value)
{
    char *string = malloc(DECDOUBLE_String);
    decDouble nativeDouble;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeDouble = decDoubleFromManaged(value);

    decDoubleToString(&nativeDouble, string);

    return string;
}

DLLEXPORT
char *nativeDecDoubleToEngString(managedDecDouble value)
{
    char *string = malloc(DECDOUBLE_String);
    decDouble nativeDouble;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    nativeDouble = decDoubleFromManaged(value);

    decDoubleToEngString(&nativeDouble, string);

    return string;
}

DLLEXPORT
managedDecDouble nativeDecDoubleFromString(char *value)
{
    decDouble nativeDouble;
    decContext context;

    decContextDefault(&context, DEC_INIT_DECDOUBLE);

    decDoubleFromString(&nativeDouble, value, &context);

    return decDoubleToManaged(nativeDouble);
}

DLLEXPORT
managedDecDouble nativeDecDoubleFromInt32(int32_t value)
{
    decDouble nativeDouble;

    decDoubleFromInt32(&nativeDouble, value);

    return decDoubleToManaged(nativeDouble);
}
