namespace Otterkit.Types;

public class ClassPrototype : AbstractPrototype
{
    public bool IsFinal;
    public List<string> Using = new();
    public Option<ClassPrototype> Inherits;

    public List<InterfacePrototype> FactoryImplements = new();
    public List<InterfacePrototype> ObjectImplements = new();

    public List<CallablePrototype> FactoryMethods = new();    
    public List<CallablePrototype> ObjectMethods = new();

    public ClassPrototype(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }
}
