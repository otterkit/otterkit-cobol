using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace Otterkit.Runtime;

public readonly struct OtterMemory
{
    internal readonly byte[] Memory;
    public readonly int Length;

    public OtterMemory()
    {
        Memory = Array.Empty<byte>();
        Length = 0;
    }

    public OtterMemory(int length)
    {
        if (length < 0) throw new ArgumentOutOfRangeException(nameof(length), "Cannot allocate less than 0 bytes.");

        Memory = new byte[length];
        Length = length;
    }

    public OtterMemory(byte[] memory)
    {
        var length = memory.Length;

        if (length < 0) throw new ArgumentOutOfRangeException(nameof(length), "Cannot allocate less than 0 bytes.");

        Memory = memory;
        Length = length;
    }

    public Span<byte> Span
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get
        {
            if (Length is 0) throw new NullReferenceException("Null memory used.");

            return Memory.AsSpan(0, Length);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start) => Span.Slice(start);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start, int length) => Span.Slice(start, length);

    public void CopyTo(Span<byte> destination) => Span.CopyTo(destination);

    public bool TryCopyTo(Span<byte> destination) => Span.TryCopyTo(destination);

    public byte[] FetchArray() => Memory;
}
