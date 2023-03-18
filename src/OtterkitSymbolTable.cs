namespace Otterkit;

public record DataSignature
{
    public CurrentScope Section;
    public string Parent = string.Empty;
    public DataSignature[] Fields = Array.Empty<DataSignature>();
    public int LevelNumber;
    public string Identifier = string.Empty;
    public string Type = string.Empty;
    public string PictureString = string.Empty;
    public int PictureLength;
    public string ExternalName = string.Empty;
    public string DefaultValue = string.Empty;
    public UsageType UsageType;
    public string UsageContext = string.Empty;
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
    public bool HasPicture;
    public bool HasValue;
}

public record SourceUnitSignature
{
    public string Identifier = string.Empty;
    public SourceUnit SourceType;
    public List<string> Parameters = new();
    public List<int> ParameterSizes = new();
    public List<bool> IsOptional = new();
    public List<bool> IsByRef = new();
    public string Returning = string.Empty;
}

public record RepositorySignature
{
    public string Identifier = string.Empty;
    public SourceUnit SourceType;
    public string ExternalizedIdentifier = string.Empty;
    public string Expands = string.Empty;
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