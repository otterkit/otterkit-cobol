using System.Runtime.InteropServices;
using System.Text;
using System.Buffers;

namespace OtterkitLibrary;

public sealed unsafe class OtterkitNativeMemory<TBytes>
    : MemoryManager<TBytes>
    where TBytes : unmanaged
{
    private TBytes* Pointer { get; set; }
    private int Length { get; init; }
    private bool Disposed = false;

    public OtterkitNativeMemory(TBytes* pointer, int length)
    {
        if (length < 0)
            throw new ArgumentOutOfRangeException(nameof(length), "Otterkit Memory Management: Cannot allocate negative bytes of memory");

        Pointer = pointer;
        Length = length;
    }

    public override Span<TBytes> GetSpan() => new(Pointer, Length);

    public override MemoryHandle Pin(int index = 0)
    {
        if (index < 0 || index >= Length)
            throw new ArgumentOutOfRangeException(nameof(index), "Otterkit Memory Management: Tried to access an index out of range of an unmanaged memory block");

        return new MemoryHandle(Pointer + index);
    }

    public override void Unpin()
    {
        Console.WriteLine("Otterkit Memory Management: Cannot unpin manualy allocated memory block");
    }

    public void Dispose() => Dispose(true);

    protected override void Dispose(bool disposing)
    {
        if (!Disposed)
        {
            if (disposing)
                Memory.Span.Clear();

            NativeMemory.Free(Pointer);
            Pointer = null;
            Disposed = true;
        }
    }
}

public sealed unsafe class BasedDataItem
{
    public Memory<byte> Memory { get; private set; }
    public OtterkitNativeMemory<byte> UnsafeMemory { get; private set; }
    public int Length { get; private set; }
    private readonly Encoding encoding = Encoding.UTF8;

    public BasedDataItem()
    {
        Length = 0;
        Memory = null;
        UnsafeMemory = new(null, 0);
    }

    public void Allocate(int length, bool initialized)
    {
        Length = length;
        byte* Pointer;

        if (initialized)
        {
            Pointer = (byte*)NativeMemory.AllocZeroed((nuint)length);
            UnsafeMemory = new(Pointer, length);
        }

        if (!initialized)
        {
            Pointer = (byte*)NativeMemory.Alloc((nuint)length);
            UnsafeMemory = new(Pointer, length);
        }

        Memory = UnsafeMemory.Memory;
    }

    public void Free()
    {
        UnsafeMemory.Dispose();
        Length = 0;
        Memory = null;
        UnsafeMemory = new(null, 0);
    }

    public void NullCheck()
    {
        if (Memory.Span == null) throw new EcDataPtrNull();
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            NullCheck();
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
        set
        {
            NullCheck();
            Memory.Span.Fill(32);

            int byteDifference = encoding.GetByteCount(value) - value.Length;

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            _ = encoding.GetBytes(value[..byteLength], Memory.Span);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            NullCheck();
            return Memory.Span;
        }
        set
        {
            NullCheck();
            Memory.Span.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value[..length].CopyTo(Memory.Span);
        }
    }

    public string Display
    {
        get
        {
            NullCheck();
            return encoding.GetString(Memory.Span);
        }
    }
}

public sealed class BasedAlphanumeric
{
    public BasedDataItem Parent { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public BasedAlphanumeric(int offset, int length, BasedDataItem parent)
    {
        this.Parent = parent;
        this.Offset = offset;
        this.Length = length;
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return MemoryMarshal.Cast<byte, char>(MemoryOffset);
        }
        set
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            _ = encoding.GetBytes(value[..byteLength], MemoryOffset);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return MemoryOffset;
        }
        set
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value[..length].CopyTo(MemoryOffset);
        }
    }

    public string Display
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return encoding.GetString(MemoryOffset);
        }
    }
}

public sealed class BasedAlphabetic
{
    public BasedDataItem Parent { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public BasedAlphabetic(int offset, int length, BasedDataItem parent)
    {
        this.Parent = parent;
        this.Offset = offset;
        this.Length = length;
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return MemoryMarshal.Cast<byte, char>(MemoryOffset);
        }
        set
        {
            if (value.IndexOfAny("1234567890") > -1)
                throw new ArgumentOutOfRangeException(nameof(value), "Alphabetic type cannot contain numberic values");

            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            _ = encoding.GetBytes(value[..byteLength], MemoryOffset);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return MemoryOffset;
        }
        set
        {
            if (value.IndexOfAny("1234567890"u8) > -1)
                throw new ArgumentOutOfRangeException(nameof(value), "Alphabetic type cannot contain numberic values");

            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value[..length].CopyTo(MemoryOffset);
        }
    }

    public string Display
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return encoding.GetString(MemoryOffset);
        }
    }
}

public sealed class BasedNational
{
    public BasedDataItem Parent { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public BasedNational(int offset, int length, BasedDataItem parent)
    {
        this.Parent = parent;
        this.Offset = offset;
        this.Length = length;
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return MemoryMarshal.Cast<byte, char>(MemoryOffset);
        }
        set
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            _ = encoding.GetBytes(value[..byteLength], MemoryOffset);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return MemoryOffset;
        }
        set
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value[..length].CopyTo(MemoryOffset);
        }
    }

    public string Display
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return encoding.GetString(MemoryOffset);
        }
    }
}

public sealed class BasedOtterkitBoolean
{
    public BasedDataItem Parent { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public BasedOtterkitBoolean(int offset, int length, BasedDataItem parent)
    {
        this.Parent = parent;
        this.Offset = offset;
        this.Length = length;
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return MemoryMarshal.Cast<byte, char>(MemoryOffset);
        }
        set
        {
            foreach (char chars in value)
            {
                if (!chars.Equals('0') || !chars.Equals('1'))
                    throw new ArgumentException("Boolean data type can only contain 0s and 1s");
            }

            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(48);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            _ = encoding.GetBytes(value[..byteLength], MemoryOffset);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return MemoryOffset;
        }
        set
        {
            foreach (byte bytes in value)
            {
                if (!bytes.Equals(0) || !bytes.Equals(1))
                    throw new ArgumentException("Boolean data type can only contain 0s and 1s");
            }

            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(48);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value[..length].CopyTo(MemoryOffset);
        }
    }

    public string Display
    {
        get
        {
            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            return encoding.GetString(MemoryOffset);
        }
    }
}
