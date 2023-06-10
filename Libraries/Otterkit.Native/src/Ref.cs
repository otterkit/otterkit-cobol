using System.Runtime.CompilerServices;

namespace Otterkit.Native;

public readonly ref struct Ref<T> where T : struct
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
    public ref T Unwrap() => ref Reference;
}
