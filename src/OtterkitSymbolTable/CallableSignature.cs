namespace Otterkit;

public class CallableSignature : AbstractSignature
{
    public List<string> Parameters = new();
    public List<int> ParameterSizes = new();
    public List<bool> IsOptional = new();
    public List<bool> IsByRef = new();
    public string Returning = "";
    public bool Override;
    public bool IsFinal;

    public CallableSignature(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }
}