using System.Runtime.CompilerServices;

namespace Otterkit.Native;

public unsafe readonly struct u8Memory
{
    /// <summary>Pointer to the allocated stack memory.</summary>
    public readonly byte* Pointer;

    /// <summary>Length of the allocated stack memory.</summary>
    public readonly int Length;

    /// <summary>Returns true if the stack pointer is not null</summary>
    public readonly bool Allocated => Pointer is not null;

    public Span<byte> Span
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get
        {
            if (!Allocated) throw new NullReferenceException("Null memory used.");

            return new Span<byte>(Pointer, Length);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start) => Span.Slice(start);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Span<byte> Slice(int start, int length) => Span.Slice(start, length);
}
