namespace Otterkit;

public class CallableSignature : AbstractSignature
{
    public LocalReferences<DataSignature> Definitions = new();
    public List<RepositorySignature> Repository = new();
    public List<DataSignature> Parameters = new();
    public List<bool> IsOptional = new();
    public List<bool> IsByRef = new();
    public DataSignature? Returning;
    public bool Override;
    public bool IsFinal;

    public CallableSignature(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }

    public (List<DataSignature>, List<bool>, List<bool>) GetParameters()
    {
        return (Parameters, IsOptional, IsByRef);
    }
}
