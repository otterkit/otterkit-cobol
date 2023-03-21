using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit;

public class LocalReferences<TValue> where TValue: notnull
{
    private readonly Dictionary<string, List<TValue>> ReferenceLookup = new(StringComparer.OrdinalIgnoreCase);

    public void AddOrUpdateReference(string localName, TValue localReference)
    {
        ref var references = ref CollectionsMarshal.GetValueRefOrAddDefault(ReferenceLookup, localName, out var exists);

        if (!exists)
        {
            references = new();
            references.Add(localReference);
        }

        if (exists && references is not null) references.Add(localReference);

        if (exists && references is null)
        {
            throw new ArgumentNullException(nameof(localName), "Reference name exists but value was null in the ReferenceLookup dictionary");
        }
    }

    public bool ReferenceExists(string localName)
    {
        ref var references = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, localName);

        if (!Unsafe.IsNullRef(ref references)) return true;

        return false;
    }

    public bool IsReferenceUnique(string localName)
    {
        ref var references = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, localName);

        if (!Unsafe.IsNullRef(ref references))
        {
            return references.Count == 1;
        }

        throw new ArgumentOutOfRangeException(nameof(localName), "Reference name does not exist in the ReferenceLookup dictionary");
    }

    public ref List<TValue>? GetNameReferences(string localName)
    {
        ref var references = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, localName);

        if (!Unsafe.IsNullRef(ref references))
        {
            return ref references;
        }

        throw new ArgumentOutOfRangeException(nameof(localName), "Reference name does not exist in the ReferenceLookup dictionary");
    }

    public void ClearReferences() 
    { 
        ReferenceLookup.Clear(); 
    }
}