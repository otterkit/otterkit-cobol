## compile win-x64: 

cl.exe /O2 /LD /Fe:..\build\nativelib.dll stackalloc.c u8console.c

## compile linux-x64: 

clang -shared -Wl,-rpath -O3 -fPIC -Wall -W -o ../build/nativelib.dylib stackalloc.c u8console.c

## compile macos-x64: 

clang -dynamiclib -O3 -Wall -W -o ../build/nativelib.dylib stackalloc.c u8console.c