using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Otterkit;

public class DataReferences
{
    private readonly Dictionary<string, List<DataSignature>> ReferenceLookup = new(StringComparer.OrdinalIgnoreCase);

    public void AddOrUpdate(string dataName, DataSignature dataSignature)
    {
        ref var references = ref CollectionsMarshal.GetValueRefOrAddDefault(ReferenceLookup, dataName, out var exists);

        if (!exists)
        {
            references = new();
            references.Add(dataSignature);
        }

        if (exists && references is not null) references.Add(dataSignature);

        if (exists && references is null)
        {
            throw new ArgumentNullException(nameof(dataName), "Data name exists but value was null in the ReferenceLookup dictionary");
        }
    }

    public bool ReferenceExists(string dataName)
    {
        ref var references = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, dataName);

        if (!Unsafe.IsNullRef(ref references)) return true;

        return false;
    }

    public bool IsReferenceUnique(string dataName)
    {
        ref var references = ref CollectionsMarshal.GetValueRefOrNullRef(ReferenceLookup, dataName);

        if (!Unsafe.IsNullRef(ref references))
        {
            return references.Count == 1;
        }

        throw new ArgumentOutOfRangeException(nameof(dataName), "Data name does not exist in the ReferenceLookup dictionary");
    }

    public void ClearReferences() { ReferenceLookup.Clear(); }
}