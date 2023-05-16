using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit.Types;

public sealed class LocalNames<TValue> where TValue: notnull
{
    private readonly Dictionary<string, TValue> NameLookup = new(StringComparer.OrdinalIgnoreCase);

    public bool TryAdd(Token name, TValue localName)
    {
        ref var names = ref CollectionsMarshal.GetValueRefOrAddDefault(NameLookup, name.Value, out var exists);

        if (!exists)
        {
            names = localName;
            
            return true;
        }

        return false;
    }

    public bool Exists(Token name)
    {
        ref var names = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, name.Value);

        if (!Unsafe.IsNullRef(ref names)) return true;

        return false;
    }
}
