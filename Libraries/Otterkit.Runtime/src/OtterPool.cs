using System.Buffers;

namespace Otterkit.Runtime;

public static class OtterPool
{
    internal static readonly ArrayPool<byte> Pool = ArrayPool<byte>.Shared;

    public static byte[] Rent(int minimumLength)
    {
        return Pool.Rent(minimumLength);
    }

    public static void Return(byte[] memory, bool clearMemory = false)
    {
        Pool.Return(memory, clearMemory);
    }
}
