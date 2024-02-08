using System.Runtime.CompilerServices;

namespace Otterkit.Native;

// This is essentially a managed pointer to a struct, so a reference to a struct.
// We can use this to avoid copying structs around, a nice performance boost.
public readonly unsafe ref struct Ref<T> where T : unmanaged
{
    internal readonly ref T Reference;
    
    public readonly bool IsNull => Unsafe.IsNullRef(ref Reference);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Ref()
    {
        Reference = ref Unsafe.NullRef<T>();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Ref(ref T reference)
    {
        Reference = ref reference;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Ref(T* pointer)
    {
        Reference = ref *(T*)pointer;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public ref T Unwrap() => ref Reference;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public T* AsPointer() => (T*)Unsafe.AsPointer(ref Reference);
}
