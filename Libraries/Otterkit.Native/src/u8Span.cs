using System.Runtime.CompilerServices;

namespace Otterkit.Native;

public unsafe struct u8Span : IDisposable
{
    private byte* Pointer;
    private int Length { get; init; }

    public bool Allocated => Pointer is not null;

    public u8Span(int length)
    {
        Pointer = Allocator.Alloc(length);
        Length = length;
    }

    public Span<byte> Span
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get => new(Pointer, Length);
    }
    
    public ref byte this[int index]
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get => ref Span[index];
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start) => Span.Slice(start);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start, int length) => Span.Slice(start, length);

    public void CopyTo(Span<byte> destination) => Span.CopyTo(destination);

    public bool TryCopyTo(Span<byte> destination) => Span.TryCopyTo(destination);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public byte* AsPointer() => Pointer;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static implicit operator Span<byte>(u8Span span) => span.Span;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static implicit operator byte*(u8Span span) => span.Pointer;

    public void Dispose()
    {
        Allocator.Dealloc(Pointer);

        Pointer = null;
    }
}
