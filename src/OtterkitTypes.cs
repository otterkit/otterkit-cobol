using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
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

public sealed class Numeric
{
    public Decimal128 dataItem;
    public int length;
    public int fractionalLength;
    public bool isSigned = false;

    public Numeric(string value, int length, int fractionalLength, bool signed)
    {
        this.dataItem = new Decimal128(value);
        this.length = length;
        this.fractionalLength = fractionalLength;
        this.isSigned = signed;
    }

    public bool isNumeric()
    {
        // return Regex.IsMatch(dataItem.Value, @"^([+-]?)(\.\d|\d\.|\d)(\d+)*$", RegexOptions.Compiled | RegexOptions.NonBacktracking);
        return true;
    }

    public bool isAlphanumeric()
    {
        return true;
    }

    public bool isAlphabetic()
    {
        return false;
    }

    public bool isNational()
    {
        return true;
    }

    public bool isBoolean()
    {
        return Regex.IsMatch(dataItem.Value, @"^([01]+)$", RegexOptions.Compiled | RegexOptions.NonBacktracking);
    }

    public string Formatted()
    {
        string abs = Decimal128.Abs(dataItem).Value;
        int indexOfDecimal = abs.IndexOf('.');

        if (indexOfDecimal < 0 && fractionalLength != 0)
            abs += ".0";

        if (indexOfDecimal >= 0 && fractionalLength == 0)
        {
            dataItem.Value = dataItem.Value.Substring(0, indexOfDecimal);
        }

        if (fractionalLength != 0)
        {
            int startIndex = (indexOfDecimal - length) < 0 ? 0 : indexOfDecimal - length;
            int endIndex = Math.Min(abs.Length, indexOfDecimal + fractionalLength + 1 - startIndex);
            int offset = length - indexOfDecimal < 0 ? 0 : length - indexOfDecimal;

            return String.Create(length + fractionalLength + 1, abs, (span, value) =>
            {
                ReadOnlySpan<char> temporary = value.AsSpan(startIndex, endIndex);
                span.Fill('0');
                temporary.CopyTo(span.Slice(offset));
            });
        }

        string padInt = abs.PadLeft(length, '0');
        // If Numeric item doesn't have a fractional value, pad missing zeros and remove overflow
        return padInt.Substring(padInt.Length - length);
    }

    public string DisplayValue
    {
        get
        {
            if (dataItem < 0 && isSigned)
            {
                return "-" + Formatted();
            }

            if (dataItem >= 0 && isSigned)
            {
                return "+" + Formatted();
            }

            return Formatted();
        }
    }
}

public sealed class Alphanumeric
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public Alphanumeric(ReadOnlySpan<char> value, int offset, int length, Memory<byte> memory)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(32);

        int byteDifference = (encoding.GetByteCount(value) - value.Length);

        int byteLength = Length < value.Length + byteDifference
            ? Length - byteDifference
            : value.Length;

        encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
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

    public Alphabetic(ReadOnlySpan<char> value, int offset, int length, Memory<byte> memory)
    {
        if(value.IndexOfAny("1234567890") > -1)
            throw new ArgumentOutOfRangeException("value" ,"Alphabetic type cannot contain numberic values");

        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(32);

        int byteDifference = (encoding.GetByteCount(value) - value.Length);

        int byteLength = Length < value.Length + byteDifference
            ? Length - byteDifference
            : value.Length;

        encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
        set
        {
            if(value.IndexOfAny("1234567890") > -1)
                throw new ArgumentOutOfRangeException("value" ,"Alphabetic type cannot contain numberic values");

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
            if(value.IndexOfAny("1234567890"u8) > -1)
                throw new ArgumentOutOfRangeException("value" ,"Alphabetic type cannot contain numberic values");
                
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
            if(value.IndexOfAny("1234567890") > -1)
                throw new ArgumentOutOfRangeException("value" ,"Alphabetic type cannot contain numberic values");

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
            if(value.IndexOfAny("1234567890"u8) > -1)
                throw new ArgumentOutOfRangeException("value" ,"Alphabetic type cannot contain numberic values");

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

    public National(ReadOnlySpan<char> value, int offset, int length, Memory<byte> memory)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(32);

        int byteDifference = (encoding.GetByteCount(value) - value.Length);

        int byteLength = Length < value.Length + byteDifference
            ? Length - byteDifference
            : value.Length;

        encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
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

public sealed class Boolean
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public Boolean(ReadOnlySpan<char> value, int offset, int length, Memory<byte> memory)
    {
        if(value.IndexOfAny("01") > -1)
            throw new ArgumentOutOfRangeException("value" ,"Boolean type can only contain 1s and 0s");

        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(48);

        int byteDifference = (encoding.GetByteCount(value) - value.Length);

        int byteLength = Length < value.Length + byteDifference
            ? Length - byteDifference
            : value.Length;

        encoding.GetBytes(value.Slice(0, byteLength), Memory.Span);
    }

    public ReadOnlySpan<char> Chars
    {
        get
        {
            return MemoryMarshal.Cast<byte, char>(Memory.Span);
        }
        set
        {
            if(value.IndexOfAny("01") > -1)
                throw new ArgumentOutOfRangeException("value" ,"Boolean type can only contain 1s and 0s");

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
            if(value.IndexOfAny("01"u8) > -1)
                throw new ArgumentOutOfRangeException("value" ,"Boolean type can only contain 1s and 0s");
                
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

public sealed class BasedBoolean
{
    public BasedDataItem Parent { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public BasedBoolean(int offset, int length, BasedDataItem parent)
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
            if(value.IndexOfAny("01") > -1)
                throw new ArgumentOutOfRangeException("value" ,"Boolean type can only contain 1s and 0s");

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
            if(value.IndexOfAny("01"u8) > -1)
                throw new ArgumentOutOfRangeException("value" ,"Boolean type can only contain 1s and 0s");

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
