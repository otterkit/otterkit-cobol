namespace Otterkit;

public static partial class SymbolTable
{
    public static readonly GlobalReferences SourceUnitGlobals = new();
    public static readonly LocalReferences<DataSignature> DataLocals = new();
    public static readonly LocalReferences<RepositorySignature> RepositoryLocals = new();

    public static TSignature GetSignature<TSignature>(string signatureName)
        where TSignature : AbstractSignature
    {
        return SourceUnitGlobals.GetSignature<TSignature>(signatureName);
    }

    public static void ClearLocalReferences()
    {
        DataLocals.ClearReferences();
        RepositoryLocals.ClearReferences();
    }
}