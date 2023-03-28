namespace Otterkit;

public class ClassSignature : AbstractSignature
{
    public bool IsFinal;
    public List<string> Using = new();
    public ClassSignature? Inherits;
    public string ExternalizedName = "";

    public Dictionary<string, InterfaceSignature> FactoryImplements = new();
    public Dictionary<string, InterfaceSignature> ObjectImplements = new();

    public Dictionary<string, CallableSignature> FactoryMethods = new();    
    public Dictionary<string, CallableSignature> ObjectMethods = new();

    public ClassSignature(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }
}