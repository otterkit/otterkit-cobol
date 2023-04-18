using System.Collections;

namespace Otterkit.Types;

public sealed class Compact<TValue> : 
    IEnumerable<TValue>
    where TValue : notnull
{
    private readonly TValue[] CompactArray;
    public int Length => CompactArray.Length;

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

    public IEnumerable<TValue> GetEnumerable()
    {
        return CompactArray;
    }

    public IEnumerator<TValue> GetEnumerator()
    {
        return GetEnumerable().GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return CompactArray.GetEnumerator();
    }
}
