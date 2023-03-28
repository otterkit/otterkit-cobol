using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit;

public sealed class GlobalReferences
{
    private readonly Dictionary<string, AbstractSignature> ReferenceLookup = new(StringComparer.OrdinalIgnoreCase);

    public bool TryAddGlobalReference(string globalName, AbstractSignature globalReference)
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

    public bool ReferenceExists<TSignature>(string globalName)
        where TSignature : AbstractSignature
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, globalName);

        if (!Unsafe.IsNullRef(ref reference)) return reference is TSignature;

        return false;
    }

    public TSignature GetSignature<TSignature>(string globalName)
        where TSignature : AbstractSignature
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, globalName);

        if (!Unsafe.IsNullRef(ref reference))
        {
            return (TSignature)reference;
        }

        throw new ArgumentNullException(nameof(globalName), "Global reference does not exist in the ReferenceLookup Dictionary");
    }
}