namespace Otterkit;

public static partial class SymbolTable
{
    internal static readonly Dictionary<string, SymbolPointer> Symbols = new(StringComparer.OrdinalIgnoreCase);
    private static readonly List<RepositorySignature> RepositorySignatures = new();
    private static readonly List<SourceUnitSignature> SourceUnitSignatures = new();
    private static readonly List<DataSignature> DataItemSymbols = new();

    public static void AddSymbol(string symbolHash, SymbolType symbolType)
    {
        int symbolIndex = default;

        if (symbolType is SymbolType.DataItem)
        {
            DataItemSymbols.Add(new DataSignature());

            symbolIndex = DataItemSymbols.Count - 1;
        }

        if (symbolType is SymbolType.RepositorySignature)
        {
            RepositorySignatures.Add(new RepositorySignature());

            symbolIndex = RepositorySignatures.Count - 1;
        }

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

    public static DataSignature GetDataItem(string symbolHash)
    {
        var pointer = GetPointer(symbolHash);

        return DataItemSymbols[pointer.SymbolIndex];
    }

    public static SourceUnitSignature GetSourceUnit(string symbolHash)
    {
        var pointer = GetPointer(symbolHash);

        return SourceUnitSignatures[pointer.SymbolIndex];
    }

    public static RepositorySignature GetRepository(string symbolHash)
    {
        var pointer = GetPointer(symbolHash);

        return RepositorySignatures[pointer.SymbolIndex];
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