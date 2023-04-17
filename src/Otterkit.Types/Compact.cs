using System.Collections;

namespace Otterkit.Types;

public sealed class Compact<TValue> : IEnumerable<TValue>
    where TValue : notnull
{
    private readonly TValue[] CompactArray;
    public int Count => CompactArray.Length;

    public Compact()
    {
        CompactArray = new TValue[1];
    }

    public Compact(int capacity)
    {
        CompactArray = new TValue[capacity];
    }

    public TValue this[int index]
    {
        get => CompactArray[index];

        set => CompactArray[index] = value;
    }

    public IEnumerator<TValue> GetEnumerator()
    {
        return ((IEnumerable<TValue>)CompactArray).GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return CompactArray.GetEnumerator();
    }
}
