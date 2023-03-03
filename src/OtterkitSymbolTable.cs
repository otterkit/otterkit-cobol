namespace Otterkit;

public record DataItemInfo
{
    public CurrentScope Section;
    public string? Parent;
    public int Line;
    public int LevelNumber;
    public string? Identifier;
    public string? Type;
    public string? PictureLength;
    public string? ExternalName;
    public string? DefaultValue;
    public UsageType UsageType;
    public string? UsageContext;
    public bool IsExternal;
    public bool IsElementary;
    public bool IsGroup;
    public bool IsConstant;
    public bool IsGlobal;
    public bool IsBased;
    public bool IsTypedef;
    public bool IsAnyLength;
    public bool IsDynamicLength;
    public bool IsRedefines;
    public bool IsRenames;
    public bool IsBlank;
    public bool IsAligned;
    public bool IsConstantRecord;
    public bool IsProperty;
    public bool IsPicture;
    public bool IsValue;
}

public record SourceUnitSignature
{
    public string? Identifier;
    public SourceUnit SourceType;
    public List<string> Parameters = new();
    public List<bool> IsOptional = new();
    public List<bool> IsByRef = new();
    public string? Returning;
}

public record RepositorySignature
{
    public string? Identifier;
    public SourceUnit SourceType;
    public string? ExternalizedIdentifier;
    public string? Expands;
    public List<string>? Using;
}

public struct SymbolPointer
{
    public required SymbolType SymbolType { get; set; }
    public required int SymbolIndex { get; set; }
}

public static class SymbolTable
{
    internal static readonly Dictionary<string, SymbolPointer> Symbols = new(StringComparer.OrdinalIgnoreCase);
    private static readonly List<RepositorySignature> RepositorySignatures = new();
    private static readonly List<SourceUnitSignature> SourceUnitSignatures = new();
    private static readonly List<DataItemInfo> DataItemSymbols = new();

    public static void AddSymbol(string symbolHash, SymbolType symbolType)
    {
        int symbolIndex = default;

        if (symbolType is SymbolType.DataItem)
        {
            DataItemSymbols.Add(new DataItemInfo());

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

    public static DataItemInfo GetDataItem(string symbolHash)
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