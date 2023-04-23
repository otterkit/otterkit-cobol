using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit.Types;

public sealed class LocalNames<TValue> where TValue: notnull
{
    private readonly Dictionary<string, List<TValue>> NameLookup = new(StringComparer.OrdinalIgnoreCase);

    public void AddEntry(Token entry, TValue localEntry)
    {
        ref var entries = ref CollectionsMarshal.GetValueRefOrAddDefault(NameLookup, entry.Value, out var exists);

        if (!exists)
        {
            entries = new(1);
            entries.Add(localEntry);
        }

        if (exists && entries is not null) entries.Add(localEntry);

        if (exists && entries is null)
        {
            throw new ArgumentException("Local entry exists but value was null in the NameLookup dictionary", nameof(entry));
        }
    }

    public bool EntryExists(Token entry)
    {
        ref var entries = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, entry.Value);

        if (!Unsafe.IsNullRef(ref entries)) return true;

        return false;
    }

    public (bool, bool) HasUniqueEntry(Token entry)
    {
        ref var entries = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, entry.Value);

        if (!Unsafe.IsNullRef(ref entries) && entries is not null)
        {
            return (true, entries.Count == 1);
        }

        return (false, false);
    }

    public List<TValue> EntriesList(Token entry)
    {
        ref var entries = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, entry.Value);

        if (!Unsafe.IsNullRef(ref entries) && entries is not null)
        {
            return entries;
        }

        throw new ArgumentOutOfRangeException(nameof(entry), "Local entry does not exist in the NameLookup dictionary");
    }

    public TValue UniqueEntry(Token entry)
    {
        ref var entries = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, entry.Value);

        if (!Unsafe.IsNullRef(ref entries) && entries is not null)
        {
            return entries[0];
        }

        throw new ArgumentOutOfRangeException(nameof(entry), "Local entry does not exist in the NameLookup dictionary");
    }
}
