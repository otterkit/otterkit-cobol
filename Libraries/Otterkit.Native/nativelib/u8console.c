#include "CASPAL.h"
#include <stdio.h>

#define RESET "\x1B[0m"
#define RESET_LF "\x1B[0m\n"

public void Write(const uint8* string)
{
    fputs((const char*)string, stdout);

    fputs(RESET, stdout);
}

public void WriteLn(const uint8* string)
{
    fputs((const char*)string, stdout);

    fputs(RESET_LF, stdout);
}

public void ReadLn(uint8* buffer, int32 length)
{
    fgets((char*)buffer, length, stdin);
}
