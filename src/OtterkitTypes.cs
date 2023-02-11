using System.Runtime.InteropServices;
using System.Text;
using System.Buffers;

namespace OtterkitLibrary;

public interface COBOLType
{
    Memory<byte> Memory { get; init; }
    ReadOnlySpan<byte> Bytes { get; set; }
    string Display { get; }
}

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

public sealed class DataItem : COBOLType
{
    public Memory<byte> Memory { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public DataItem(int length)
    {
        Length = length;
        Memory = new byte[length];
    }

    public DataItem(Memory<byte> memory)
    {
        Length = memory.Length;
        Memory = memory;
    }

    public DataItem(int length, Memory<byte> memory)
    {
        Length = length;
        Memory = memory;
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
            return Memory.Span;
        }
        set
        {
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

public sealed class Constant
{
    public Memory<byte> Memory { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public Constant(ReadOnlySpan<byte> bytes)
    {
        this.Memory = bytes.ToArray();
    }

    public Constant(string bytes)
    {
        this.Memory = encoding.GetBytes(bytes);
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

public sealed class Numeric : COBOLType, IComparable<Numeric>
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    public int FractionalLength { get; init; }
    public bool IsInteger { get; private set; }
    public bool IsSigned { get; private set; }
    public bool IsNegative { get; private set; }
    private readonly Encoding encoding = Encoding.UTF8;

    public Numeric(ReadOnlySpan<byte> value, int offset, int length, int fractionalLength, Memory<byte> memory)
    {
        this.IsSigned = value[0] == 43 || value[0] == 45;
        this.Offset = offset;
        this.Length = length;
        this.FractionalLength = fractionalLength;
        this.IsInteger = fractionalLength == 0;

        if (fractionalLength == 0)
        {
            int signedSpace = IsSigned ? 1 : 0;
            this.Memory = memory.Slice(offset, length + signedSpace);
        }

        if (fractionalLength > 0)
        {
            int signedSpace = IsSigned ? 2 : 1;
            this.Memory = memory.Slice(offset, length + fractionalLength + signedSpace);
        }

        Memory.Span.Fill(48);

        if (IsSigned)
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
        this.IsSigned = isSigned;
        this.IsInteger = fractionalLength == 0;

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
            int minusSignOffset = decimalHolder.Bytes[0] != 45 ? 0 : 1;

            this.Length = isSigned ? DecimalPointIndex - minusSignOffset : DecimalPointIndex;
            this.FractionalLength = isSigned ? decimalHolder.Bytes.Length - Length - (minusSignOffset + 1) : decimalHolder.Bytes.Length - Length - 1;
        }
        else
        {
            if (decimalHolder.Bytes[0] is 45 or 43)
            {
                this.Length = decimalHolder.Bytes.Length - 1;
            }
            else
            {
                this.Length = decimalHolder.Bytes.Length;
            }

            this.FractionalLength = 0;
        }

        this.IsSigned = isSigned;
        this.IsInteger = FractionalLength == 0;

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

        if (!IsInteger && indexOfDecimal < 0)
        {
            indexOfDecimal = bytes.Length;
            formatted[Length + (IsSigned ? 1 : 0)] = 46;
        }

        int startIndex;
        if (!IsInteger || indexOfDecimal > -1)
        {
            startIndex = Math.Max(0, indexOfDecimal - Length);
        }
        else
        {
            startIndex = Math.Max(0, bytes.Length - Length);
        }
        
        int endIndex;
        if (!IsInteger) 
        {
            endIndex = Math.Min(bytes.Length - startIndex, Length + FractionalLength + 1);
        }
        else
        {
            endIndex = indexOfDecimal < 0
            ? Math.Min(Length, bytes.Length)
            : Math.Min(Length, indexOfDecimal);
        }
        
        int offset;
        if (indexOfDecimal < 0)
        {
            offset = Math.Max(0, Memory.Length - bytes.Length);
        }
        else
        {
            offset = Math.Max(0, Length - indexOfDecimal);
        }

        if (isSigned)
        {
            offset++;
        }

        endIndex += startIndex;

        bytes[startIndex..endIndex].CopyTo(formatted[offset..]);

        int indexOfSign = formatted.IndexOfAny("+-"u8);
        if (indexOfSign > -1) formatted[indexOfSign] = 48;

        if (!isSigned)
        {
            formatted.CopyTo(Memory.Span);
            return;
        }

        formatted.CopyTo(Memory.Span);
        Memory.Span[0] = (byte)(IsNegative ? 45 : 43);
    }

    private void FormatSigned(ReadOnlySpan<byte> bytes)
    {
        if (bytes[0] == 45)
        {
            IsNegative = true;
            Format(bytes, true);
            return;
        }

        if (bytes[0] != 43)
        {
            Span<byte> withSign = stackalloc byte[bytes.Length + 1];
            bytes.CopyTo(withSign[1..]);
            withSign[0] = 43;
            IsNegative = false;
            Format(withSign, true);
            return;
        }

        IsNegative = false;
        Format(bytes, true);
    }

    public static bool operator >(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec > Rdec);
    }

    public static bool operator <(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec < Rdec);
    }

    public static bool operator <=(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec <= Rdec);
    }

    public static bool operator >=(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec >= Rdec);
    }

    public static bool operator ==(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec == Rdec);
    }

    public static bool operator !=(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        return (Ldec != Rdec);
    }

    public static Numeric operator +(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec + Rdec;

        Numeric result = new Numeric(Dres, true);

        return result;
    }

    public static Numeric operator ++(Numeric number)
    {
        DecimalHolder num = number;

        num = num++;

        Numeric result = new Numeric(num, true);

        return result;
    }

    public static Numeric operator -(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec - Rdec;

        Numeric result = new Numeric(Dres, true);

        return result;
    }

    public static Numeric operator -(Numeric number)
    {
        DecimalHolder num = number;

        num = -num;

        Numeric result = new Numeric(num, true);

        return result;
    }

    public static Numeric operator --(Numeric number)
    {
        DecimalHolder num = number;

        num = num--;

        Numeric result = new Numeric(num, true);

        return result;
    }

    public static Numeric operator *(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec * Rdec;

        Numeric result = new Numeric(Dres, true);

        return result;
    }

    public static Numeric operator /(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec / Rdec;

        Numeric result = new Numeric(Dres, true);

        return result;
    }

    public static Numeric operator %(Numeric left, Numeric right)
    {
        DecimalHolder Ldec = left;
        DecimalHolder Rdec = right;

        DecimalHolder Dres = Ldec % Rdec;

        Numeric result = new Numeric(Dres, true);

        return result;
    }

    public override bool Equals(object? obj)
    {
        if (obj == null || GetType() != obj.GetType())
        {
            return false;
        }
        
        return this == (Numeric)obj;
    }

    public int CompareTo(Numeric? other){ //for implementing default C# sorting
        if (other is null || this > other){
            return 1;
        } else if (this.Equals(other)){
            return 0;
        } else {
            return -1;
        }
    }
    
    public override int GetHashCode()
    {
        return Memory.Span[0].GetHashCode();
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return Memory.Span;
        }
        set
        {
            if (IsSigned)
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

public sealed class Alphanumeric : COBOLType
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

        value[..byteLength].CopyTo(Memory.Span);
    }

    public Alphanumeric(Memory<byte> memory, int offset, int length)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
    }

    public Alphanumeric(ReadOnlySpan<byte> aphanumeric)
    {
        this.Length = aphanumeric.Length;
        this.Memory = new byte[Length];
        aphanumeric.CopyTo(Memory.Span);
    }

    public static bool operator >(Alphanumeric left, Alphanumeric right)
    {
        int unequalPosition = left.Bytes.CommonPrefixLength(right.Bytes);
        return left.Bytes[unequalPosition] > right.Bytes[unequalPosition];
    }

    public static bool operator <(Alphanumeric left, Alphanumeric right)
    {
        int unequalPosition = left.Bytes.CommonPrefixLength(right.Bytes);
        return left.Bytes[unequalPosition] < right.Bytes[unequalPosition];
    }

    public static bool operator >=(Alphanumeric left, Alphanumeric right)
    {
        int unequalPosition = left.Bytes.CommonPrefixLength(right.Bytes);
        return left.Bytes[unequalPosition] >= right.Bytes[unequalPosition];
    }

    public static bool operator <=(Alphanumeric left, Alphanumeric right)
    {
        int unequalPosition = left.Bytes.CommonPrefixLength(right.Bytes);
        return left.Bytes[unequalPosition] <= right.Bytes[unequalPosition];
    }

    public static bool operator ==(Alphanumeric left, Alphanumeric right)
    {
        return left.Bytes.SequenceEqual(right.Bytes);
    }

    public static bool operator !=(Alphanumeric left, Alphanumeric right)
    {
        return !left.Bytes.SequenceEqual(right.Bytes);
    }

    public override bool Equals(object? obj)
    {
        throw new NotSupportedException();
    }

    public override int GetHashCode()
    {
        throw new NotSupportedException();
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

            value[..length].CopyTo(Memory.Span);
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

public sealed class Alphabetic : COBOLType
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public Alphabetic(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory)
    {
        if (value.IndexOfAny("1234567890"u8) > -1)
            throw new ArgumentOutOfRangeException(nameof(value), "Alphabetic type cannot contain numberic values");

        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
        Memory.Span.Fill(32);

        int byteLength = Length < value.Length
            ? Length
            : value.Length;

        value[..byteLength].CopyTo(Memory.Span);
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
                throw new ArgumentOutOfRangeException(nameof(value), "Alphabetic type cannot contain numberic values");

            Memory.Span.Fill(32);

            int byteDifference = (encoding.GetByteCount(value) - value.Length);

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
            return Memory.Span;
        }
        set
        {
            if (value.IndexOfAny("1234567890"u8) > -1)
                throw new ArgumentOutOfRangeException(nameof(value), "Alphabetic type cannot contain numberic values");

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

public sealed class National : COBOLType
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

        value[..byteLength].CopyTo(Memory.Span);
    }

    public National(Memory<byte> memory, int offset, int length)
    {
        this.Offset = offset;
        this.Length = length;
        this.Memory = memory.Slice(offset, length);
    }

    public National(ReadOnlySpan<byte> national)
    {
        this.Length = national.Length;
        this.Memory = new byte[Length];
        national.CopyTo(Memory.Span);
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

            _ = encoding.GetBytes(value[..byteLength], Memory.Span);
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

            value[..length].CopyTo(Memory.Span);
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

public sealed class COBOLBoolean : COBOLType
{
    public Memory<byte> Memory { get; init; }
    public int Offset { get; init; }
    public int Length { get; init; }
    private readonly Encoding encoding = Encoding.UTF8;

    public COBOLBoolean(ReadOnlySpan<byte> value, int offset, int length, Memory<byte> memory)
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

        value[..byteLength].CopyTo(Memory.Span);
    }

    public COBOLBoolean(Memory<byte> memory, int offset, int length)
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

            _ = encoding.GetBytes(value[..byteLength], Memory.Span);
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

            value[..length].CopyTo(Memory.Span);
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
