using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit.Types;

public sealed class GlobalNames
{
    private readonly Dictionary<string, AbstractPrototype> NameLookup = new(StringComparer.OrdinalIgnoreCase);

    public bool TryAddName(string prototypeName, AbstractPrototype prototype)
    {
        ref var prototypeRef = ref CollectionsMarshal.GetValueRefOrAddDefault(NameLookup, prototypeName, out var exists);

        if (!exists)
        {
            prototypeRef = prototype;
            return true;
        }

        return false;
    }

    public bool NameExists(string prototypeName)
    {
        ref var prototypeRef = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, prototypeName);

        if (!Unsafe.IsNullRef(ref prototypeRef)) return true;

        return false;
    }

    public bool NameExists<TPrototype>(string prototypeName)
        where TPrototype : AbstractPrototype
    {
        ref var prototypeRef = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, prototypeName);

        if (!Unsafe.IsNullRef(ref prototypeRef)) return prototypeRef is TPrototype;

        return false;
    }

    public TPrototype GetPrototype<TPrototype>(string prototypeName)
        where TPrototype : AbstractPrototype
    {
        ref var prototypeRef = ref CollectionsMarshal.GetValueRefOrNullRef(NameLookup, prototypeName);

        if (!Unsafe.IsNullRef(ref prototypeRef))
        {
            return (TPrototype)prototypeRef;
        }

        throw new ArgumentNullException(nameof(prototypeName), "Global prototype does not exist in the PrototypeLookup Dictionary");
    }
}
