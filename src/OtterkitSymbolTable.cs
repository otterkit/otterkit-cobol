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
    public List<string>? UsageContext;
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
    public string? Returning;
    public List<string>? Parameters;
    public List<bool>? IsOptional;
    public List<bool>? IsByRef;
    public List<string>? Exceptions;
    public List<SourceUnitSignature>? Methods;
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
        if (Symbols.ContainsKey(symbolHash))
        {
            return Symbols[symbolHash];
        }

        throw new ArgumentException($"Symbol table key \"{symbolHash}\" does not exist.");
    }

    public static bool CheckType(SymbolPointer symbolPointer, SymbolType symbolType)
    {
        if (symbolPointer.SymbolType == symbolType) return true;

        return false;
    }

    // TODO: The following classes need to be removed and
    // converted into the new symbol table functionality
    public static class DataItems
    {
        internal static readonly Dictionary<string, DataItemInfo> Data = new(StringComparer.OrdinalIgnoreCase);

        public static DataItemInfo GetValue(string DataItemHash)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (!AlreadyExists)
                throw new ArgumentException($"FAILED TO GET DATA ITEM HASH FOR {DataItemHash}: THIS SHOULD NOT HAVE HAPPENED, PLEASE REPORT THIS ISSUE ON OTTERKIT'S REPO");
            
            return Data[DataItemHash];
        }

        public static bool ValueExists(string DataItemHash)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);
            return AlreadyExists;
        }

        public static bool AddDataItem(string DataItemHash, string Identifier, int LevelNumber, Token token)
        {
            DataItemInfo DataItem = new()
            {
                LevelNumber = LevelNumber,
                Identifier = Identifier,
                Line = token.line
            };
            
            Data.Add(DataItemHash, DataItem);
            return true;
        }

        public static bool AddType(string DataItemHash, string Type)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.Type = Type;
                Data[DataItemHash] = DataItem;
                return true;
            }
                
            return false;
        }

        public static bool AddPicture(string DataItemHash, string Picture)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.PictureLength = Picture;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool AddDefault(string DataItemHash, string Default)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.DefaultValue = Default;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool AddUsage(string DataItemHash, UsageType usageType, params string[] usageContext)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.UsageType = usageType;
                if (usageContext.Length >= 1)
                    DataItem.UsageContext = usageContext.ToList();
                    
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool AddSection(string DataItemHash, CurrentScope Section)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.Section = Section;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool AddParent(string DataItemHash, string Parent)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.Parent = Parent;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool IsExternal(string DataItemHash, bool IsExternal, string ExternalName)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsExternal = IsExternal;
                DataItem.ExternalName = ExternalName;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool IsConstant(string DataItemHash, bool IsConstant)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsConstant = IsConstant;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool IsGlobal(string DataItemHash, bool IsGlobal)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsGlobal = IsGlobal;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool IsElementary(string DataItemHash, bool IsElementary)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsElementary = IsElementary;
                DataItem.IsGroup = false;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool IsGroup(string DataItemHash, bool IsGroup)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsGroup = IsGroup;
                DataItem.IsElementary = false;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }

        public static bool IsPicture(string DataItemHash, bool IsPicture)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsPicture = IsPicture;
                Data[DataItemHash] = DataItem;
                return true;
            }

            return false;
        }
    }

    public static class SourceUnits
    {
        internal static readonly Dictionary<string, SourceUnitSignature> Data = new(StringComparer.OrdinalIgnoreCase);

        public static SourceUnitSignature GetValue(string SourceUnitHash)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out _);

            if (!AlreadyExists)
                throw new ArgumentException($"FAILED TO GET SOURCE UNIT HASH FOR {SourceUnitHash}: THIS SHOULD NOT HAVE HAPPENED, PLEASE REPORT THIS ISSUE ON OTTERKIT'S REPO");
            
            return Data[SourceUnitHash];
        }

        public static bool ValueExists(string SourceUnitHash)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out _);
            return AlreadyExists;
        }

        public static bool AddSourceUnit(string SourceUnitHash, string identifier, SourceUnit SourceType)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out _);

            if (AlreadyExists)
                return false;

            SourceUnitSignature sourceUnitSignature = new()
            {
                Identifier = identifier,
                SourceType = SourceType,
                Methods = new(),
                Exceptions = new(),
                Parameters = new(),
                IsByRef = new(),
                IsOptional = new()
            };

            Data.Add(SourceUnitHash, sourceUnitSignature);
            return true;
        }

        public static bool AddReturning(string SourceUnitHash, string Returning)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out _);

            if (AlreadyExists)
            {
                Data[SourceUnitHash].Returning = Returning;
            }
                
            return false;
        }

        public static bool AddParameter(string SourceUnitHash, string Parameter, bool IsOptional, bool IsByRef)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out _);

            if (AlreadyExists)
            {
                SourceUnitSignature Source = Data[SourceUnitHash];
                Source.Parameters!.Add(Parameter);
                Source.IsOptional!.Add(IsOptional);
                Source.IsByRef!.Add(IsByRef);
            }
                
            return false;
        }

        public static bool AddException(string SourceUnitHash, string exception)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out _);

            if (AlreadyExists)
            {
                SourceUnitSignature Source = Data[SourceUnitHash];
                Source.Exceptions!.Add(exception);
            }
                
            return false;
        }

        public static bool AddMethod(string SourceUnitHash, SourceUnitSignature Method)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out _);

            if (AlreadyExists)
            {
                SourceUnitSignature Source = Data[SourceUnitHash];
                Source.Methods!.Add(Method);
            }
                
            return false;
        }

    }

    public static class Repositories
    {
        internal static readonly Dictionary<string, RepositorySignature> Data = new(StringComparer.OrdinalIgnoreCase);

        public static RepositorySignature GetValue(string RepositoryHash)
        {
            bool AlreadyExists = Data.TryGetValue(RepositoryHash, out _);

            if (!AlreadyExists)
                throw new ArgumentException($"FAILED TO GET REPOSITORY HASH FOR {RepositoryHash}: THIS SHOULD NOT HAVE HAPPENED, PLEASE REPORT THIS ISSUE ON OTTERKIT'S REPO");
            
            return Data[RepositoryHash];
        }

        public static bool ValueExists(string RepositoryHash)
        {
            bool AlreadyExists = Data.TryGetValue(RepositoryHash, out _);
            return AlreadyExists;
        }

        public static bool AddToRepository(string RepositoryHash, string Identifier, SourceUnit SourceType, string ExternalizedIdentifier, string Expands, params string[] Using)
        {
            bool AlreadyExists = Data.TryGetValue(RepositoryHash, out _);

            if (AlreadyExists) return false;

            RepositorySignature Repository = new();
            Repository.Identifier = Identifier;
            Repository.SourceType = SourceType;
            Repository.ExternalizedIdentifier = ExternalizedIdentifier;
            Repository.Expands = Expands;

            Repository.Using = new();
            foreach (var parameter in Using)
            {
                Repository.Using.Add(parameter);
            }

            Data.Add(RepositoryHash, Repository);
            return true;
        }

    }
}

