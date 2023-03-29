namespace Otterkit;

public class ClassSignature : AbstractSignature
{
    public bool IsFinal;
    public List<string> Using = new();
    public ClassSignature? Inherits;

    public List<InterfaceSignature> FactoryImplements = new();
    public List<InterfaceSignature> ObjectImplements = new();

    public List<CallableSignature> FactoryMethods = new();    
    public List<CallableSignature> ObjectMethods = new();

    public ClassSignature(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }
}
