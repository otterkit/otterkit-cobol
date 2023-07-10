#include "common.h"
#include <stdio.h>

#define RESET "\x1B[0m"
#define RESET_LF "\x1B[0m\n"

public void Write(const char ref string)
{
    fputs(string, stdout);

    fputs(RESET, stdout);
}

public void WriteLn(const char ref string)
{
    fputs(string, stdout);

    fputs(RESET_LF, stdout);
}

public void ReadLn(char ref buffer, i32 length)
{
    fgets(buffer, length, stdin);
}
