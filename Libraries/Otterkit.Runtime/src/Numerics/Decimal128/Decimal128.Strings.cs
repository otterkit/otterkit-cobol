using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

public partial struct Decimal128
{
    public static Decimal128 Parse(ReadOnlySpan<byte> utf8String)
    {
        return Decimal128Bindings.FromString(MemoryMarshal.GetReference(utf8String));
    }
    
    public static Decimal128 Parse(ReadOnlySpan<char> stringValue)
    {
        var length = Encoding.UTF8.GetByteCount(stringValue);
        
        Span<byte> span = stackalloc byte[length];

        Encoding.UTF8.GetBytes(stringValue, span);
        
        return Decimal128Bindings.FromString(MemoryMarshal.GetReference(span));
    }
    
    public unsafe ReadOnlyMemory<byte> ToUtf8Memory()
    {
        // This is a C string (char*), becomes a byte* in C#.
        var pointer = Decimal128Bindings.ToString(this);

        // Because C strings are null terminated.
        var span = MemoryMarshal.CreateReadOnlySpanFromNullTerminated(pointer);

        // Create a new byte array to hold a copy of the C string.
        // This is necessary to convert the unsafe malloc allocated
        // buffer into a managed buffer that we can safely return.
        var copy = new byte[span.Length];
        
        span.CopyTo(copy);

        // The pointer must be freed before returning, otherwise
        // we'll get a memory leak from not freeing a malloc
        // allocated C string.
        NativeMemory.Free(pointer);
        
        // Return the copy
        return copy; 
    }

    public unsafe int AsSpan(Span<byte> destination)
    {
        // This is a C string (char*), becomes a byte* in C#.
        var pointer = Decimal128Bindings.ToString(this);
        
        // Because C strings are null terminated.
        var span = MemoryMarshal.CreateReadOnlySpanFromNullTerminated(pointer);
        
        // Copy to destination because we cannot safely
        // return a new span without allocating heap memory.
        span.CopyTo(destination);
        
        // The pointer must be freed before returning, otherwise
        // we'll get a memory leak from not freeing a malloc
        // allocated C string.
        NativeMemory.Free(pointer);

        // Return the length of the copied span.
        return span.Length;        
    }
    
    public unsafe byte* AsNullTerminatedPointer()
    {
        // This is a C string (char*), becomes a byte* in C#.
        var pointer = Decimal128Bindings.ToString(this);

        // Return the C string directly
        // WARNING: This pointer needs to be freed after
        // otherwise we'll get a memory leak
        return pointer;        
    }

    public override unsafe string ToString()
    {
        // This is a C string (char*), becomes a byte* in C#.
        var pointer = Decimal128Bindings.ToString(this);

        // Because C strings are null terminated.
        var span = MemoryMarshal.CreateReadOnlySpanFromNullTerminated(pointer);

        // Create C# string from a C string.
        var outString = Encoding.UTF8.GetString(span);

        // The pointer must be freed before returning, otherwise
        // we'll get a memory leak from not freeing a malloc
        // allocated C string.
        NativeMemory.Free(pointer);

        return outString;
    }

    public unsafe string ToEngineeringString()
    {
        // This is a C string (char*), becomes a byte* in C#.
        var pointer = Decimal128Bindings.ToString(this);

        // Because C strings are null terminated.
        var span = MemoryMarshal.CreateReadOnlySpanFromNullTerminated(pointer);

        // Create C# string from a C string.
        var outString = Encoding.UTF8.GetString(span);

        // The pointer must be freed before returning, otherwise
        // we'll get a memory leak from not freeing a malloc
        // allocated C string.
        NativeMemory.Free(pointer);

        return outString;
    }
}
