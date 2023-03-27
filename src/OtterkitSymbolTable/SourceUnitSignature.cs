namespace Otterkit;

public class SourceUnitSignature
{
    public Token Identifier;
    public SourceUnit SourceType;
    public List<string> Parameters = new();
    public List<int> ParameterSizes = new();
    public List<bool> IsOptional = new();
    public List<bool> IsByRef = new();
    public string Returning = "";

    public SourceUnitSignature(Token identifier, SourceUnit sourcetype)
    {
        Identifier = identifier;
        SourceType = sourcetype;
    }
}
