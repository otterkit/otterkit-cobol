using System.Collections;
using System.Diagnostics.CodeAnalysis;

namespace Otterkit;

public sealed class Compact<TValue> : IEnumerable<TValue>
{
    private readonly TValue[] CompactList;
    public int Count => CompactList.Length;

    public Compact()
    {
        CompactList = new TValue[1];
    }

    public Compact(int capacity)
    {
        CompactList = new TValue[capacity];
    }

    public TValue this[int index]
    {
        get => CompactList[index];

        set => CompactList[index] = value;
    }

    public IEnumerator<TValue> GetEnumerator()
    {
        return ((IEnumerable<TValue>)CompactList).GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return CompactList.GetEnumerator();
    }
}
