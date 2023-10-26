## compile win-x64:

"C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvars64.bat"

cl.exe /O2 /LD /Fe:..\build\nativelib.dll allocator.c u8console.c decNumber.c ..\decNumber\decContext.c ..\decNumber\decDouble.c ..\decNumber\decQuad.c ..\decNumber\decNumber.c ..\decNumber\decimal128.c ..\decNumber\decimal64.c

## compile linux-x64:

clang -shared -Wl,-rpath -O3 -fPIC -Wall -W -o ../build/nativelib.so *.c decNumber.c ../decNumber/decContext.c ../decNumber/decDouble.c ../decNumber/decQuad.c ../decNumber/decNumber.c ../decNumber/decimal128.c ../decNumber/decimal64.c

## compile macos-x64:

clang -dynamiclib -O3 -std=c11 -pedantic -Wall -Wextra -Werror -o ../build/nativelib.dylib *.c ../decNumber/decContext.c ../decNumber/decDouble.c ../decNumber/decQuad.c ../decNumber/decNumber.c ../decNumber/decimal128.c ../decNumber/decimal64.c -march=native
