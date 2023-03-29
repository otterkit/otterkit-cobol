namespace Otterkit;

public static partial class SymbolTable
{
    public static readonly GlobalReferences SourceUnits = new();

    public static TSignature GetSignature<TSignature>(string signatureName)
        where TSignature : AbstractSignature
    {
        return SourceUnits.GetSignature<TSignature>(signatureName);
    }

    public static bool TryAddName(string globalName, AbstractSignature globalSignature, bool isResolutionPass)
    {
        return SourceUnits.TryAddGlobalName(globalName, globalSignature, isResolutionPass);
    }
}