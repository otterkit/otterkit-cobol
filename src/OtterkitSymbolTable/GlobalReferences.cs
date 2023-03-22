using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit;

public sealed class GlobalReferences<TValue> where TValue: notnull
{
    private readonly Dictionary<string, TValue> ReferenceLookup = new(StringComparer.OrdinalIgnoreCase);

    public void AddGlobalReference(string globalName, TValue globalReference)
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrAddDefault(ReferenceLookup, globalName, out var exists);

        if (!exists)
        {
            reference = globalReference;
        }

        if (exists)
        {
            throw new ArgumentException("Global reference name already exists in the ReferenceLookup dictionary", nameof(globalName));
        }
    }

    public bool ReferenceExists(string globalName)
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, globalName);

        if (!Unsafe.IsNullRef(ref reference)) return true;

        return false;
    }

    public TValue GetGlobalReferenceByName(string globalName)
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, globalName);

        if (!Unsafe.IsNullRef(ref reference) && reference is not null)
        {
            return reference;
        }

        throw new ArgumentException("Global reference name does not exist in the ReferenceLookup dictionary", nameof(globalName));
    }
}