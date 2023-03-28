using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit;

public sealed class GlobalReferences<TValue> where TValue: notnull
{
    private readonly Dictionary<string, TValue> ReferenceLookup = new(StringComparer.OrdinalIgnoreCase);

    public bool TryAddGlobalReference(string globalName, TValue globalReference)
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrAddDefault(ReferenceLookup, globalName, out var exists);

        if (!exists)
        {
            reference = globalReference;
            return true;
        }

        return false;
    }

    public bool ReferenceExists(string globalName)
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, globalName);

        if (!Unsafe.IsNullRef(ref reference)) return true;

        return false;
    }

    public bool ReferenceExists<TGlobal>(string globalName)
        where TGlobal : class
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, globalName);

        if (!Unsafe.IsNullRef(ref reference)) return reference is TGlobal;

        return false;
    }

    public TGlobal? GetSignature<TGlobal>(string globalName)
        where TGlobal : class
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, globalName);

        if (!Unsafe.IsNullRef(ref reference))
        {
            return reference as TGlobal;
        }

        return null;
    }
}