namespace Otterkit.Types;

public class InterfacePrototype : AbstractPrototype
{
    public bool IsFinal;
    public List<string> Using = new();
    public List<ClassPrototype> Inherits = new();

    // Interface prototype methods
    public List<CallablePrototype> Methods = new();

    public InterfacePrototype(Token identifier, SourceUnit sourcetype)
        : base (identifier, sourcetype) { }
}