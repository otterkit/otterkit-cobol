#define DECUSE64 1

#include "..\decNumber\decQuad.h" // decQuad library
#include <stdio.h>

#ifdef _WIN32
  #define DLLEXPORT __declspec(dllexport)
#endif

#ifndef _WIN32
  #define DLLEXPORT
#endif

// compile win-x64: cl.exe /O2 /LD decQuadBindings.c ..\decNumber\decContext.c ..\decNumber\decQuad.c

typedef struct
{
  uint64_t _upperBits;
  uint64_t _lowerBits;
} managedDecQuad;

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

DLLEXPORT
managedDecQuad nativeDecQuadAdd(managedDecQuad left, managedDecQuad right)
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

DLLEXPORT
managedDecQuad nativeDecQuadSub(managedDecQuad left, managedDecQuad right)
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

DLLEXPORT
char *nativeDecQuadToString(managedDecQuad value)
{
  static char string[DECQUAD_String];
  decQuad nativeQuad;
  decContext context;

  decContextDefault(&context, DEC_INIT_DECQUAD);

  nativeQuad = decQuadFromManaged(value);

  decQuadToString(&nativeQuad, string);

  return string;
}

DLLEXPORT
managedDecQuad nativeDecQuadFromString(char *value)
{
  decQuad nativeQuad;
  decContext context;

  decContextDefault(&context, DEC_INIT_DECQUAD);

  decQuadFromString(&nativeQuad, value, &context);

  return decQuadToManaged(nativeQuad);
}
