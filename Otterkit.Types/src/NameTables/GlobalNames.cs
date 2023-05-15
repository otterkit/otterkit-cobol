using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit.Types;

public sealed class GlobalNames
{
    private readonly Dictionary<string, AbstractPrototype> NameLookup = new(StringComparer.OrdinalIgnoreCase);

    public bool TryAdd(Token nameToken, AbstractPrototype prototype)
    {
        ref var prototypeRef = ref CollectionsMarshal.GetValueRefOrAddDefault(NameLookup, nameToken.Value, out var exists);

        if (!exists)
        {
            prototypeRef = prototype;
            return true;
        }

        return false;
    }

    public bool Exists(Token nameToken)
    {
        ref var prototypeRef = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, nameToken.Value);

        if (!Unsafe.IsNullRef(ref prototypeRef)) return true;

        return false;
    }

    public bool Exists<TPrototype>(Token nameToken)
        where TPrototype : AbstractPrototype
    {
        ref var prototypeRef = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, nameToken.Value);

        if (!Unsafe.IsNullRef(ref prototypeRef)) return prototypeRef is TPrototype;

        return false;
    }

    public AbstractPrototype Fetch(Token nameToken)
    {
        ref var prototypeRef = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, nameToken.Value);

        if (!Unsafe.IsNullRef(ref prototypeRef))
        {
            return prototypeRef;
        }

        throw new ArgumentNullException(nameof(nameToken), "Global prototype does not exist in the NameLookup Dictionary");
    }

    public TPrototype Fetch<TPrototype>(Token nameToken)
        where TPrototype : AbstractPrototype
    {
        ref var prototypeRef = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, nameToken.Value);

        if (!Unsafe.IsNullRef(ref prototypeRef))
        {
            return (TPrototype)prototypeRef;
        }

        throw new ArgumentNullException(nameof(nameToken), "Global prototype does not exist in the NameLookup Dictionary");
    }
}
