namespace Otterkit;

public static partial class SymbolTable
{
    internal static readonly Dictionary<string, SymbolPointer> Symbols = new(StringComparer.OrdinalIgnoreCase);
    private static readonly List<SourceUnitSignature> SourceUnitSignatures = new();
    public static readonly GlobalReferences<SourceUnitSignature> SourceUnitGlobals = new();
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

        if (symbolType is SymbolType.SourceUnitSignature)
        {
            SourceUnitSignatures.Add(new SourceUnitSignature());

            symbolIndex = SourceUnitSignatures.Count - 1;
        }

        SymbolPointer pointer = new()
        {
            SymbolType = symbolType,
            SymbolIndex = symbolIndex
        };

        Symbols.Add(symbolHash, pointer);
    }

    public static SourceUnitSignature GetSourceUnit(string symbolHash)
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

    public static bool CheckType(SymbolPointer symbolPointer, SymbolType symbolType)
    {
        if (symbolPointer.SymbolType == symbolType) return true;

        return false;
    }
}