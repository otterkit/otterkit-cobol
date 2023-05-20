using System.Runtime.InteropServices;

namespace Otterkit.Native;

public static partial class StackAllocator
{
    [LibraryImport("nativelib", EntryPoint = "alloc")]
    internal static partial StackMemory Alloc(ulong length);

    [LibraryImport("nativelib", EntryPoint = "dealloc")]
    internal static partial void Dealloc(StackMemory memory);
}
