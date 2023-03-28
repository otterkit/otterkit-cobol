namespace Otterkit;

public static partial class SymbolTable
{
    internal static readonly Dictionary<string, SymbolPointer> Symbols = new(StringComparer.OrdinalIgnoreCase);
    private static readonly List<CallableSignature> SourceUnitSignatures = new();
    public static readonly GlobalReferences<AbstractSignature> SourceUnitGlobals = new();
    public static readonly LocalReferences<RepositorySignature> RepositoryLocals = new();
    public static readonly LocalReferences<DataSignature> DataLocals = new();

    public static void ClearLocalReferences()
    {
        DataLocals.ClearReferences();
        RepositoryLocals.ClearReferences();
    }

    public static void AddSymbol(string symbolHash, SymbolType symbolType)
    {
        int symbolIndex = default;

        SymbolPointer pointer = new()
        {
            SymbolType = symbolType,
            SymbolIndex = symbolIndex
        };

        Symbols.Add(symbolHash, pointer);
    }

    public static CallableSignature GetSourceUnit(string symbolHash)
    {
        var pointer = GetPointer(symbolHash);

        return SourceUnitSignatures[pointer.SymbolIndex];
    }

    public static bool SymbolExists(string symbolHash)
    {
        if (Symbols.ContainsKey(symbolHash)) return true;

        return false;
    }

    public static SymbolPointer GetPointer(string symbolHash)
    {
        if (Symbols.TryGetValue(symbolHash, out SymbolPointer value))
        {
            return value;
        }

        throw new ArgumentException($"Symbol table key \"{symbolHash}\" does not exist.");
    }

}