namespace Otterkit.Types;

public class CallablePrototype : AbstractPrototype
{
    public LocalEntries<RepositoryEntry> RepositoryEntries = new();
    public LocalEntries<FileControlEntry> FileEntries = new();
    public LocalEntries<DataEntry> DataEntries = new();
    public List<DataEntry> Parameters = new();
    public List<bool> IsOptional = new();
    public List<bool> IsByRef = new();
    public Option<DataEntry> Returning;
    public bool Override;
    public bool IsFinal;

    public CallablePrototype(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }

    public (List<DataEntry>, List<bool>, List<bool>) GetParameters()
    {
        return (Parameters, IsOptional, IsByRef);
    }
}
