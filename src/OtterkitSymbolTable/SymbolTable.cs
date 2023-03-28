namespace Otterkit;

public static partial class SymbolTable
{
    public static readonly GlobalReferences<AbstractSignature> SourceUnitGlobals = new();
    public static readonly LocalReferences<RepositorySignature> RepositoryLocals = new();
    public static readonly LocalReferences<DataSignature> DataLocals = new();

    public static void ClearLocalReferences()
    {
        DataLocals.ClearReferences();
        RepositoryLocals.ClearReferences();
    }

    public static TGlobal? GetSignature<TGlobal>(string signatureName)
        where TGlobal : AbstractSignature
    {
        return SourceUnitGlobals.GetSignature<TGlobal>(signatureName);
    }
}