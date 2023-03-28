namespace Otterkit;

public class InterfaceSignature : AbstractSignature
{
    public bool IsFinal;
    public List<string> Using = new();
    public List<ClassSignature> Inherits = new();
    public string ExternalizedName = "";

    // Interface prototype methods
    public List<CallableSignature> Methods = new();

    public InterfaceSignature(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }
}