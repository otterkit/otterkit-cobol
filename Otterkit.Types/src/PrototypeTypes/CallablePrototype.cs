namespace Otterkit.Types;

// Don't use a separate class or struct for this,
// a ValueTuple is enough, and gives us extra syntax sugar
using ParameterTuple = ValueTuple<DataEntry, bool, bool>;

public class CallablePrototype : AbstractPrototype
{
    public LocalNames<RepositoryEntry> RepositoryEntries = new();
    public LocalNames<FileControlEntry> FileEntries = new();
    public LocalNames<DataEntry> DataEntries = new();
    public List<ParameterTuple> Parameters = new();
    public Option<DataEntry> Returning;
    public bool Override;
    public bool IsFinal;

    public CallablePrototype(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }
}
