using System.Runtime.InteropServices;

namespace Otterkit.Native;

public static partial class StackAllocator
{
    [LibraryImport("nativelib", EntryPoint = "alloc")]
    public static partial StackMemory Alloc(ulong length);

    [LibraryImport("nativelib", EntryPoint = "dealloc")]
    public static partial void Dealloc(StackMemory memory);
}
