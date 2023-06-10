using System.Runtime.CompilerServices;

namespace Otterkit.Native;

public unsafe struct u8Memory : IDisposable
{
    private Pointer Pointer;

    public int Length => Pointer.Length;
    public bool Allocated => Pointer.Allocated;

    public u8Memory(int length)
    {
        Pointer.Alloc(length);
    }

    public Span<byte> Span
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get
        {
            if (!Pointer.Allocated) throw new NullReferenceException("Null pointer used.");

            return new Span<byte>(Pointer.Value, Pointer.Length);
        }
    }
    
    public ref byte this[int index] => ref Span[index];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start) => Span.Slice(start);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start, int length) => Span.Slice(start, length);

    public void CopyTo(Span<byte> destination) => Span.CopyTo(destination);

    public bool TryCopyTo(Span<byte> destination) => Span.TryCopyTo(destination);

    public Ref<u8Memory> AsRef() => new Ref<u8Memory>(ref Unsafe.AsRef(this));

    public void Dispose()
    {
        if (Pointer.Allocated) Pointer.Dealloc();
    }
}
