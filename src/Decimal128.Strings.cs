using System.Runtime.InteropServices;
using System.Text;

namespace Otterkit.Numerics;

public readonly partial struct Decimal128
{
    public static Decimal128 FromString(ReadOnlySpan<byte> utf8String)
    {
        return DecQuadBindings.FromString(MemoryMarshal.GetReference(utf8String));
    }

    public unsafe byte[] ToUtf8Array()
    {
        // This is a C string (char*), becomes a byte* in C#.
        var pointer = DecQuadBindings.ToString(this);

        var length = 0;
        var current = pointer[0];

        // Look for the C string null terminator.
        while (current != 0)
        {
            current = pointer[length];
            length++;
        }

        // Create UTF-8 ROS<byte> from the C string.
        var span = new ReadOnlySpan<byte>(pointer, length);

        // Create a new byte array to hold a copy of the C string.
        // This is necessary to convert the unsafe malloc allocated
        // buffer into a managed buffer that we can safely return.
        var copy = new byte[length];
        
        span.CopyTo(copy);

        // The pointer must be freed before returning, otherwise
        // we'll get a memory leak from not freeing a malloc
        // allocated C string.
        NativeMemory.Free(pointer);
        
        // Return the copy
        return copy;        
    }

    public ReadOnlySpan<byte> ToUtf8Span()
    {
        return ToUtf8Array();
    }

    public ReadOnlyMemory<byte> ToUtf8Memory()
    {
        return ToUtf8Array();
    }

    public unsafe ReadOnlySpan<byte> AsUnsafeSpan(out byte* nativePointer)
    {
        // This is a C string (char*), becomes a byte* in C#.
        var pointer = DecQuadBindings.ToString(this);

        var length = 0;
        var current = pointer[0];

        // Look for the C string null terminator.
        while (current != 0)
        {
            current = pointer[length];
            length++;
        }

        // Return C string pointer as an out parameter.
        // Warning: The caller must free this pointer.
        nativePointer = pointer;

        // Return UTF-8 ROS<byte> created from the C string.
        return new ReadOnlySpan<byte>(pointer, length);        
    }

    public override unsafe string ToString()
    {
        // This is a C string (char*), becomes a byte* in C#.
        var pointer = DecQuadBindings.ToString(this);

        var length = 0;
        var current = pointer[0];

        // Look for the C string null terminator.
        while (current != 0)
        {
            current = pointer[length];
            length++;
        }

        // Create C# string from a null terminated C string.
        var outString = Encoding.UTF8.GetString(pointer, length);

        // The pointer must be freed before returning, otherwise
        // we'll get a memory leak from not freeing a malloc
        // allocated C string.
        NativeMemory.Free(pointer);

        return outString;
    }

    public unsafe string ToEngineeringString()
    {
        // This is a C string (char*), becomes a byte* in C#.
        var pointer = DecQuadBindings.ToString(this);

        var length = 0;
        var current = pointer[0];

        // Look for the C string null terminator.
        while (current != 0)
        {
            current = pointer[length];
            length++;
        }

        // Create C# string from a null terminated C string.
        var outString = Encoding.UTF8.GetString(pointer, length);

        // The pointer must be freed before returning, otherwise
        // we'll get a memory leak from not freeing a malloc
        // allocated C string.
        NativeMemory.Free(pointer);

        return outString;
    }
}
