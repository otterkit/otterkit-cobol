using System.Runtime.InteropServices;

namespace Otterkit.Runtime;

public static unsafe partial class u8Strings
{
    [LibraryImport("nativelib", EntryPoint = "u8ScalarAlgorithm")]
    private static partial int ScalarAlgorithm(byte* input, int length);

    [LibraryImport("nativelib", EntryPoint = "u8RangeAlgorithm")]
    private static partial int RangeAlgorithm(byte* input, int length);

    [LibraryImport("nativelib", EntryPoint = "u8RangeAlgorithmAvx2")]
    private static partial int RangeAlgorithmAvx2(byte* input, int length);

    public static bool ScalarValidate(ReadOnlySpan<byte> input)
    {
        fixed (byte* ptr = input)
        {
            return ScalarAlgorithm(ptr, input.Length) == 0;
        }
    }

    public static bool SimdValidate(ReadOnlySpan<byte> input)
    {
        fixed (byte* ptr = input)
        {
            return RangeAlgorithm(ptr, input.Length) == 0;
        }
    }

    public static bool Avx2Validate(ReadOnlySpan<byte> input)
    {
        fixed (byte* ptr = input)
        {
            return RangeAlgorithmAvx2(ptr, input.Length) == 0;
        }
    }

    // [LibraryImport("nativelib", EntryPoint = "FetchTests")]
    // public static partial byte* FetchTests(int type, int index);

    // public static bool TestValidate(int type, int index)
    // {
    //     var span = MemoryMarshal.CreateReadOnlySpanFromNullTerminated(FetchTests(type, index));

    //     return Validate(span);
    // }

    // public static bool TestValidateAvx2(int type, int index)
    // {
    //     var span = MemoryMarshal.CreateReadOnlySpanFromNullTerminated(FetchTests(type, index));

    //     return ValidateAvx2(span);
    // }
}
