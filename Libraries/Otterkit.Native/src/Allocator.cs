using System.Runtime.InteropServices;

namespace Otterkit.Native;

public static partial class Allocator
{
    [LibraryImport("nativelib", EntryPoint = "alloc")]
    public static partial u8Memory Alloc(int length);

    [LibraryImport("nativelib", EntryPoint = "dealloc")]
    public static partial void Dealloc(u8Memory memory);
}
