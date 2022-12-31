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
            throw new ArgumentOutOfRangeException("Otterkit Memory Management: Cannot allocate negative bytes of memory");

        this.Pointer = pointer;
        this.Length = length;
    }

    public override Span<TBytes> GetSpan() => new Span<TBytes>(Pointer, Length);

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
                this.Memory.Span.Clear();

            NativeMemory.Free(Pointer);
            Pointer = null;
            Disposed = true;
        }
    }
}

public sealed class DataItem
{
    public Memory<byte> Memory { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public DataItem(int length)
    {
        this.Length = length;
        this.Memory = new byte[length];
    }

    public DataItem(Memory<byte> memory)
    {
        this.Length = memory.Length;
        this.Memory = memory;
    }

    public DataItem(int length, Memory<byte> memory)
    {
        this.Length = length;
        this.Memory = memory;
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
        set
        {
            Memory.Span.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            Memory.Span.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value.Slice(0, length).CopyTo(Memory.Span);
        }
    }

    public string Display
    {
        get
        {
            return encoding.GetString(Memory.Span);
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
        this.Length = 0;
        this.Memory = null;
        this.UnsafeMemory = new(null, 0);
    }

    public void Allocate(int length, bool initialized)
    {
        this.Length = length;
        byte* Pointer;

        if (initialized)
        {
            Pointer = (byte*)NativeMemory.AllocZeroed((nuint)length);
            this.UnsafeMemory = new(Pointer, length);
            Pointer = null;
        }

        if (!initialized)
        {
            Pointer = (byte*)NativeMemory.Alloc((nuint)length);
            this.UnsafeMemory = new(Pointer, length);
            Pointer = null;
        }

        this.Memory = UnsafeMemory.Memory;
    }

    public void Free()
    {
        UnsafeMemory.Dispose();
        this.Length = 0;
        this.Memory = null;
        this.UnsafeMemory = new(null, 0);
    }

    public void NullCheck()
    {
        if (this.Memory.Span == null)
            throw new EcDataPtrNull();
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

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
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

            value.Slice(0, length).CopyTo(Memory.Span);
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

public sealed class Constant
{
    public ReadOnlyMemory<byte> Memory { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public Constant(ReadOnlySpan<byte> bytes)
    {
        this.Memory = bytes.ToArray();
    }

    public Constant(string bytes)
    {
        this.Memory = encoding.GetBytes(bytes);
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
    }

    public string Display
    {
        get
        {
            return encoding.GetString(Memory.Span);
        }
    }
}

public sealed class Numeric
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    public int FractionalLength { get; init; }
    public bool isSigned { get; private set; }
    public bool isNegative { get; private set; }
    private readonly Encoding encoding = Encoding.UTF8;

    public Numeric(ReadOnlySpan<byte> value, int offset, int length, int fractionalLength, Memory<byte> memory)
    {
        this.isSigned = value[0] == 43 || value[0] == 45;
        this.Offset = offset;
        this.Length = length;
        this.FractionalLength = fractionalLength;
        if (fractionalLength == 0)
        {
            int signedSpace = isSigned ? 1 : 0;
            this.Memory = memory.Slice(offset, length + signedSpace);
        }

        if (fractionalLength > 0)
        {
            int signedSpace = isSigned ? 2 : 1;
            this.Memory = memory.Slice(offset, length + fractionalLength + signedSpace);
        }

        Memory.Span.Fill(48);

        if (isSigned)
        {
            FormatSigned(value);
            return;
        }

        Format(value);
    }

    public Numeric(Memory<byte> memory, int offset, int length, int fractionalLength, bool isSigned)
    {
        this.Offset = offset;
        this.Length = length;
        this.FractionalLength = fractionalLength;
        this.isSigned = isSigned;

        if (fractionalLength == 0)
        {
            int signedSpace = isSigned ? 1 : 0;
            this.Memory = memory.Slice(offset, length + signedSpace);
        }

        if (fractionalLength > 0)
        {
            int signedSpace = isSigned ? 2 : 1;
            this.Memory = memory.Slice(offset, length + fractionalLength + signedSpace);
        }
    }

    public Numeric(DecimalHolder decimalHolder, bool isSigned)
    {
        this.Offset = 0;

        int DecimalPointIndex = decimalHolder.Bytes.IndexOf("."u8);

        if (DecimalPointIndex >= 0)
        {
            this.Length = isSigned ? DecimalPointIndex : DecimalPointIndex;
            this.FractionalLength = decimalHolder.Bytes.Length - Length - 1;
        }
        else
        {
            this.Length = isSigned ? decimalHolder.Bytes.Length : decimalHolder.Bytes.Length;
            this.FractionalLength = 0;
        }

        this.isSigned = isSigned;

        if (this.FractionalLength == 0)
        {
            int signedSpace = isSigned ? 1 : 0;
            this.Memory = new byte[Length + signedSpace];
        }

        if (this.FractionalLength > 0)
        {
            int signedSpace = isSigned ? 2 : 1;
            this.Memory = new byte[Length + FractionalLength + signedSpace];
        }

        Memory.Span.Fill(48);

        if (isSigned)
        {
            FormatSigned(decimalHolder.Bytes);
            return;
        }

        Format(decimalHolder.Bytes);
    }

    private void Format(ReadOnlySpan<byte> bytes, bool isSigned = false)
    {
        Span<byte> formatted = stackalloc byte[Memory.Length];
        formatted.Fill(48);

        int indexOfDecimal = bytes.IndexOf("."u8);

        int isDecimal = Math.Min(FractionalLength, 1);
        int startIndex = Math.Max(0, indexOfDecimal - Length);
        int endIndex = Math.Min(bytes.Length - startIndex, Length + FractionalLength + isDecimal);
        
        ReadOnlySpan<byte> temporary = bytes.Slice(startIndex, endIndex);

        int offset = Math.Max(0, Length - indexOfDecimal) + (isSigned ? 1 : 0);
        if (indexOfDecimal < 0) offset = Math.Max(0, Length - bytes.Length) + (isSigned ? 1 : 0);

        temporary.CopyTo(formatted.Slice(offset));

        int indexOfSign = formatted.IndexOfAny("+-"u8);
        if (indexOfSign > -1)
            formatted[indexOfSign] = 48;

        if (isSigned)
        {
            formatted.CopyTo(Memory.Span);
            byte sign = (byte)(isNegative ? 45 : 43);
            Memory.Span[0] = sign;
            return;
        }

        formatted.CopyTo(Memory.Span);
    }

    private void FormatSigned(ReadOnlySpan<byte> bytes)
    {
        if (bytes[0] == 45)
        {
            isNegative = true;
            Format(bytes, true);
            return;
        }

        isNegative = false;
        Format(bytes, true);
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
        set
        {
            Span<byte> bytes = stackalloc byte[value.Length];
            encoding.GetBytes(value, bytes);
            if (isSigned)
            {
                FormatSigned(bytes);
                return;
            }

            Format(bytes);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            if (isSigned)
            {
                FormatSigned(value);
                return;
            }

            Format(value);
        }
    }

    public string Display
    {
        get
        {
            return encoding.GetString(Memory.Span);
        }
    }
}

public sealed class Alphanumeric
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public Alphanumeric(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value.Slice(0, byteLength).CopyTo(Memory.Span);
    }

    public Alphanumeric(Memory<byte> memory, int offset, int length)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
        set
        {
            Memory.Span.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            Memory.Span.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value.Slice(0, length).CopyTo(Memory.Span);
        }
    }

    public string Display
    {
        get
        {
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

            encoding.GetBytes(value.Slice(0, byteLength), MemoryOffset);
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

            value.Slice(0, length).CopyTo(MemoryOffset);
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

public sealed class Alphabetic
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public Alphabetic(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory)
    {
        if (value.IndexOfAny("1234567890"u8) > -1)
            throw new ArgumentOutOfRangeException("value", "Alphabetic type cannot contain numberic values");

        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value.Slice(0, byteLength).CopyTo(Memory.Span);
    }

    public Alphabetic(Memory<byte> memory, int offset, int length)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
        set
        {
            if (value.IndexOfAny("1234567890") > -1)
                throw new ArgumentOutOfRangeException("value", "Alphabetic type cannot contain numberic values");

            Memory.Span.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            if (value.IndexOfAny("1234567890"u8) > -1)
                throw new ArgumentOutOfRangeException("value", "Alphabetic type cannot contain numberic values");

            Memory.Span.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value.Slice(0, length).CopyTo(Memory.Span);
        }
    }

    public string Display
    {
        get
        {
            return encoding.GetString(Memory.Span);
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
                throw new ArgumentOutOfRangeException("value", "Alphabetic type cannot contain numberic values");

            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            encoding.GetBytes(value.Slice(0, byteLength), MemoryOffset);
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
                throw new ArgumentOutOfRangeException("value", "Alphabetic type cannot contain numberic values");

            Span<byte> MemoryOffset = Parent.Memory.Slice(Offset, Length).Span;
            MemoryOffset.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value.Slice(0, length).CopyTo(MemoryOffset);
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

public sealed class National
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public National(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value.Slice(0, byteLength).CopyTo(Memory.Span);
    }

    public National(Memory<byte> memory, int offset, int length)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
        set
        {
            Memory.Span.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            Memory.Span.Fill(32);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value.Slice(0, length).CopyTo(Memory.Span);
        }
    }

    public string Display
    {
        get
        {
            return encoding.GetString(Memory.Span);
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

            encoding.GetBytes(value.Slice(0, byteLength), MemoryOffset);
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

            value.Slice(0, length).CopyTo(MemoryOffset);
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

public sealed class OtterkitBoolean
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public OtterkitBoolean(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory)
    {
        foreach (byte bytes in value)
        {
            if (!bytes.Equals(0) || !bytes.Equals(1))
                throw new ArgumentException("Boolean data type can only contain 0s and 1s");
        }

        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value.Slice(0, byteLength).CopyTo(Memory.Span);
    }

    public OtterkitBoolean(Memory<byte> memory, int offset, int length)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
        set
        {
            foreach (char chars in value)
            {
                if (!chars.Equals('0') || !chars.Equals('1'))
                    throw new ArgumentException("Boolean data type can only contain 0s and 1s");
            }
            Memory.Span.Fill(48);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

            int byteLength = Length < value.Length + byteDifference
                ? Length - byteDifference
                : value.Length;

            encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
        }
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            foreach (byte bytes in value)
            {
                if (!bytes.Equals(0) || !bytes.Equals(1))
                    throw new ArgumentException("Boolean data type can only contain 0s and 1s");
            }
            Memory.Span.Fill(48);

            int length = Length < value.Length
            ? Length
            : value.Length;

            value.Slice(0, length).CopyTo(Memory.Span);
        }
    }

    public string Display
    {
        get
        {
            return encoding.GetString(Memory.Span);
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

            encoding.GetBytes(value.Slice(0, byteLength), MemoryOffset);
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

            value.Slice(0, length).CopyTo(MemoryOffset);
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
