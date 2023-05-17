using System.Buffers;
using System.Text;
using Otterkit.Numerics;

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

    public static implicit operator Decimal128(Temporary value)
    {
        return Decimal128.Parse(value.Bytes);
    }

    public static implicit operator Temporary(Decimal128 value)
    {
        Span<byte> result = stackalloc byte[45];

        var length = value.AsSpan(result);

        return new Temporary(result.Slice(0, length));
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
