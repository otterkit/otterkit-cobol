using System.Runtime.InteropServices;

namespace Otterkit.Native;

public static unsafe partial class Allocator
{
    public static int StackUsage => GetStackUsage();
    public static int StackFree => GetStackFree();

    [LibraryImport("nativelib", EntryPoint = "GetStackUsage")]
    private static partial int GetStackUsage();

    [LibraryImport("nativelib", EntryPoint = "GetStackFree")]
    private static partial int GetStackFree();

    [LibraryImport("nativelib", EntryPoint = "Alloc")]
    public static partial byte* Alloc(int length);

    [LibraryImport("nativelib", EntryPoint = "Dealloc")]
    public static partial void Dealloc(byte* memory);
}
