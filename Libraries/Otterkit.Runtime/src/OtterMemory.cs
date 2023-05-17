using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace Otterkit.Runtime;

public unsafe sealed class OtterMemory : IDisposable
{
    internal void* Pointer;

    public int Length { get; init; }
    public bool Allocated => Pointer is not null;

    public OtterMemory()
    {
        Pointer = null;
        Length = 0;
    }

    public OtterMemory(int length)
    {
        if (length < 0) throw new ArgumentOutOfRangeException(nameof(length), "Cannot allocate less than 0 bytes.");

        Pointer = NativeMemory.Alloc((nuint)length);

        Length = length;
    }

    public OtterMemory(byte* pointer, int length)
    {
        if (length < 0) throw new ArgumentOutOfRangeException(nameof(length), "Cannot allocate less than 0 bytes.");

        Pointer = pointer;
        Length = length;
    }

    public Span<byte> Span
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get
        {
            if (!Allocated) throw new NullReferenceException("Null pointer used.");

            return new Span<byte>(Pointer, Length);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start) => Span.Slice(start);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start, int length) => Span.Slice(start, length);

    public void CopyTo(Span<byte> destination) => Span.CopyTo(destination);

    public bool TryCopyTo(Span<byte> destination) => Span.TryCopyTo(destination);

    public byte[] ToArray() => Span.ToArray();

    public void Dispose()
    {
        NativeMemory.Free(Pointer);

        Pointer = null;
    }
}
