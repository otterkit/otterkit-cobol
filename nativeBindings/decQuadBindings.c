#define DECUSE64 1

#include "..\decNumber\decQuad.h" // decQuad library
#include "marshalDecQuad.h"
#include <stdio.h> // for printf

#ifdef _WIN32
  #define DLLEXPORT __declspec(dllexport)
#endif

#ifndef _WIN32
  #define DLLEXPORT
#endif

// compile win-x64: cl.exe /O2 /LD decQuadBindings.c ..\decNumber\decContext.c ..\decNumber\decQuad.c

DLLEXPORT
marshalDecQuad nativeDecQuadAdd(marshalDecQuad left, marshalDecQuad right)
{
  decQuad nativeLeft, nativeRight;
  marshalDecQuad result;
  decContext context;

  decContextDefault(&context, DEC_INIT_DECQUAD);

  nativeLeft.longs[0] = left._upperBits;
  nativeLeft.longs[1] = left._lowerBits;

  nativeRight.longs[0] = right._upperBits;
  nativeRight.longs[1] = right._lowerBits;

  decQuadAdd(&nativeLeft, &nativeLeft, &nativeRight, &context);

  result._upperBits = nativeLeft.longs[0];
  result._lowerBits = nativeLeft.longs[1];

  return result;
}

DLLEXPORT
marshalDecQuad nativeDecQuadSub(marshalDecQuad left, marshalDecQuad right)
{
  decQuad nativeLeft, nativeRight;
  marshalDecQuad result;
  decContext context;

  decContextDefault(&context, DEC_INIT_DECQUAD);

  nativeLeft.longs[0] = left._upperBits;
  nativeLeft.longs[1] = left._lowerBits;

  nativeRight.longs[0] = right._upperBits;
  nativeRight.longs[1] = right._lowerBits;

  decQuadSubtract(&nativeLeft, &nativeLeft, &nativeRight, &context);

  result._upperBits = nativeLeft.longs[0];
  result._lowerBits = nativeLeft.longs[1];

  return result;
}

DLLEXPORT
char *nativeDecQuadToString(marshalDecQuad value)
{
  static char string[DECQUAD_String];
  decQuad nativeQuad;
  decContext context;

  decContextDefault(&context, DEC_INIT_DECQUAD);

  nativeQuad.longs[0] = value._upperBits;
  nativeQuad.longs[1] = value._lowerBits;

  decQuadToString(&nativeQuad, string);

  return string;
}

DLLEXPORT
marshalDecQuad nativeDecQuadFromString(char *value)
{
  decQuad nativeQuad;
  marshalDecQuad result;
  decContext context;

  decContextDefault(&context, DEC_INIT_DECQUAD);

  decQuadFromString(&nativeQuad, value, &context);

  result._upperBits = nativeQuad.longs[0];
  result._lowerBits = nativeQuad.longs[1];

  return result;
}
