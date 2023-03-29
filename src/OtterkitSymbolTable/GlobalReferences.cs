using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit;

public sealed class GlobalReferences
{
    private readonly Dictionary<string, AbstractSignature> ReferenceLookup = new(StringComparer.OrdinalIgnoreCase);

    public bool TryAddGlobalName(string globalName, AbstractSignature globalSignature, bool isResolutionPass)
    {
        if (isResolutionPass) return true;

        ref var reference = ref CollectionsMarshal.GetValueRefOrAddDefault(ReferenceLookup, globalName, out var exists);

        if (!exists)
        {
            reference = globalSignature;
            return true;
        }

        return false;
    }

    public bool NameExists(string globalName)
    {
        ref var reference = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, globalName);

        if (!Unsafe.IsNullRef(ref reference)) return true;

        return false;
    }

    public bool NameExists<TSignature>(string globalName)
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