using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace Otterkit.Native;

[StructLayout(LayoutKind.Sequential)]
public unsafe struct Pointer
{
    /// <summary>Pointer to the allocated memory.</summary>
    public byte* Value;

    /// <summary>Length of the allocated memory in bytes.</summary>
    public int Length;

    /// <summary>True if the pointer is not null</summary>
    public bool Allocated => Value is not null;

    public Ref<Pointer> Ref => new Ref<Pointer>(ref Unsafe.AsRef(this));

    public void Alloc(int length)
    {
        this = Allocator.Alloc(length);
    }

    public void Dealloc()
    {
        Allocator.Dealloc(this);

        Value = null;
    }
}
