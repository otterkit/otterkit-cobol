#include "common.h"
#include <stdio.h>

#define RESET "\x1B[0m"
#define RESET_LF "\x1B[0m\n"

_export void write(const char *string)
{
    fputs(string, stdout);

    fputs(RESET, stdout);
}

_export void writeln(const char *string)
{
    fputs(string, stdout);

    fputs(RESET_LF, stdout);
}
