using System.Buffers;
using System.Text;

namespace Otterkit.Runtime;

public readonly struct Temporary : ICOBOLType, IDisposable
{
    private static readonly ArrayPool<byte> Pool = ArrayPool<byte>.Shared;
    public readonly byte[] ByteArray;
    public readonly int Length;

    public Temporary(ReadOnlySpan<byte> bytes)
    {
        Length = bytes.Length;

        ByteArray = Pool.Rent(Length);

        bytes.CopyTo(ByteArray);
    }

    public ReadOnlySpan<byte> Bytes
    {
        get
        {
            return ByteArray.AsSpan(0, Length);
        }
        set
        {
            return;
        }
    }

    public string Display
    {
        get
        {
            var span = ByteArray.AsSpan(0, Length);

            return Encoding.UTF8.GetString(span);
        }
    }

    public void Dispose()
    {
        Pool.Return(ByteArray);
    }
}
