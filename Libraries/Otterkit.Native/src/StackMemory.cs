using System.Runtime.CompilerServices;

namespace Otterkit.Native;

public unsafe readonly struct StackMemory
{
    /// <summary>Pointer to the allocated stack memory.</summary>
    public readonly byte* Pointer;

    /// <summary>Length of the allocated stack memory.</summary>
    public readonly ulong Length;

    /// <summary>Returns true if the stack pointer is not null</summary>
    public readonly bool Allocated => Pointer is not null;

    public Span<byte> Span
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get
        {
            if (Length is 0) throw new NullReferenceException("Null memory used.");

            // Length is a ulong because size_t is a ulong.
            // The allocator will never allocate more than int.MaxValue,
            // meaning that this should always be within the int32 range.
            var length = (int)Length;

            return new Span<byte>(Pointer, length);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start) => Span.Slice(start);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start, int length) => Span.Slice(start, length);
}
