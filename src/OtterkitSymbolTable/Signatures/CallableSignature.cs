namespace Otterkit;

public class CallableSignature : AbstractSignature
{
    public LocalReferences<EntryDefinition> Definitions = new();
    public List<RepositoryDefinition> Repository = new();
    public List<EntryDefinition> Parameters = new();
    public List<bool> IsOptional = new();
    public List<bool> IsByRef = new();
    public EntryDefinition? Returning;
    public bool Override;
    public bool IsFinal;

    public CallableSignature(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }

    public (List<EntryDefinition>, List<bool>, List<bool>) GetParameters()
    {
        return (Parameters, IsOptional, IsByRef);
    }
}
