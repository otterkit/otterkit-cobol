namespace Otterkit;

public struct DataItemInfo
{
    public CurrentScope Section;
    public string Parent;
    public int Line;
    public int LevelNumber;
    public string Identifier;
    public string Type;
    public string PictureLength;
    public string ExternalName;
    public string DefaultValue;
    public bool IsExternal;
    public bool IsElementary;
    public bool IsGroup;
    public bool IsConstant;
    public bool IsGlobal;
    public bool IsBased;
}

public struct SourceUnitSignature
{
    public string Identifier;
    public SourceUnit SourceType;
    public string Returning;
    public List<string> Parameters;
    public List<bool> IsOptional;
    public List<bool> IsByRef;
    public List<Exception> Exceptions;
    public List<string> Properties;
    public List<SourceUnitSignature> Methods;
}

public struct RepositorySignature
{
    public string Identifier;
    public SourceUnit SourceType;
    public string ExternalizedIdentifier;
    public string Expands;
    public List<string> Using;
}

public static class Information
{
    public static class DataItems
    {
        internal static readonly Dictionary<string, DataItemInfo> Data = new();

        public static DataItemInfo GetValue(string DataItemHash)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItemInfo DataItem);

            if (!AlreadyExists)
                throw new ArgumentException($"FAILED TO GET DATA ITEM HASH FOR {DataItemHash}: THIS SHOULD NOT HAVE HAPPENED, PLEASE REPORT THIS ISSUE ON OTTERKIT'S REPO");
            
            return DataItem;
        }

        public static bool ValueExists(string DataItemHash)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);
            return AlreadyExists;
        }

        public static bool AddDataItem(string DataItemHash, string Identifier, int LevelNumber, Token token)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItemInfo DataItem);

            if (AlreadyExists)
                return false;

            DataItem.LevelNumber = LevelNumber;
            DataItem.Identifier = Identifier;
            DataItem.Line = token.line;
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
            }

            return false;
        }

        public static bool IsExternal(string DataItemHash, bool IsExternal, string ExternalName)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsExternal = true;
                DataItem.ExternalName = ExternalName;
                Data[DataItemHash] = DataItem;
            }

            return false;
        }

        public static bool IsConstant(string DataItemHash, bool IsConstant)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsConstant = true;
                Data[DataItemHash] = DataItem;
            }

            return false;
        }

        public static bool IsGlobal(string DataItemHash, bool IsGlobal)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsGlobal = true;
                Data[DataItemHash] = DataItem;
            }

            return false;
        }

        public static bool IsElementary(string DataItemHash, bool IsElementary)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsElementary = true;
                Data[DataItemHash] = DataItem;
            }

            return false;
        }

        public static bool IsGroup(string DataItemHash, bool IsGroup)
        {
            bool AlreadyExists = Data.TryGetValue(DataItemHash, out _);

            if (AlreadyExists)
            {
                DataItemInfo DataItem = Data[DataItemHash];
                DataItem.IsGroup = true;
                Data[DataItemHash] = DataItem;
            }

            return false;
        }
    }

    public static class SourceUnits
    {
        internal static readonly Dictionary<string, SourceUnitSignature> Data = new();

        public static SourceUnitSignature GetValue(string SourceUnitHash)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out SourceUnitSignature DataItem);

            if (!AlreadyExists)
                throw new ArgumentException($"FAILED TO GET SOURCE UNIT HASH FOR {SourceUnitHash}: THIS SHOULD NOT HAVE HAPPENED, PLEASE REPORT THIS ISSUE ON OTTERKIT'S REPO");
            
            return DataItem;
        }

        public static bool ValueExists(string SourceUnitHash)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out _);
            return AlreadyExists;
        }

        public static bool AddSourceUnit(string SourceUnitHash, string Identifier, SourceUnit SourceType)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out SourceUnitSignature sourceUnitSignature);

            if (AlreadyExists)
                return false;

            sourceUnitSignature.Identifier = Identifier;
            sourceUnitSignature.SourceType = SourceType;

            sourceUnitSignature.Methods = new();
            sourceUnitSignature.Properties = new();
            sourceUnitSignature.Exceptions = new();
            sourceUnitSignature.Parameters = new();
            sourceUnitSignature.IsByRef = new();
            sourceUnitSignature.IsOptional = new();

            Data.Add(SourceUnitHash, sourceUnitSignature);
            return true;
        }

        public static bool AddReturning(string SourceUnitHash, string Returning)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out SourceUnitSignature Source);

            if (AlreadyExists)
            {
                Source.Returning = Returning;
                Data[SourceUnitHash] = Source;
            }
                
            return false;
        }

        public static bool AddParameter(string SourceUnitHash, string Parameter, bool IsOptional, bool IsByRef)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out SourceUnitSignature Source);

            if (AlreadyExists)
            {
                Source.Parameters.Add(Parameter);
                Source.IsOptional.Add(IsOptional);
                Source.IsByRef.Add(IsByRef);
                Data[SourceUnitHash] = Source;
            }
                
            return false;
        }

        public static bool AddException(string SourceUnitHash, Exception exception)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out SourceUnitSignature Source);

            if (AlreadyExists)
            {
                Source.Exceptions.Add(exception);
                Data[SourceUnitHash] = Source;
            }
                
            return false;
        }

        public static bool AddProperty(string SourceUnitHash, string Property)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out SourceUnitSignature Source);

            if (AlreadyExists)
            {
                Source.Properties.Add(Property);
                Data[SourceUnitHash] = Source;
            }
                
            return false;
        }

        public static bool AddMethod(string SourceUnitHash, SourceUnitSignature Method)
        {
            bool AlreadyExists = Data.TryGetValue(SourceUnitHash, out SourceUnitSignature Source);

            if (AlreadyExists)
            {
                Source.Methods.Add(Method);
                Data[SourceUnitHash] = Source;
            }
                
            return false;
        }

    }

    public static class Repositories
    {
        internal static readonly Dictionary<string, RepositorySignature> Data = new();

        public static RepositorySignature GetValue(string RepositoryHash)
        {
            bool AlreadyExists = Data.TryGetValue(RepositoryHash, out RepositorySignature Repository);

            if (!AlreadyExists)
                throw new ArgumentException($"FAILED TO GET REPOSITORY HASH FOR {RepositoryHash}: THIS SHOULD NOT HAVE HAPPENED, PLEASE REPORT THIS ISSUE ON OTTERKIT'S REPO");
            
            return Repository;
        }

        public static bool ValueExists(string RepositoryHash)
        {
            bool AlreadyExists = Data.TryGetValue(RepositoryHash, out _);
            return AlreadyExists;
        }

        public static bool AddToRepository(string RepositoryHash, string Identifier, SourceUnit SourceType, string ExternalizedIdentifier, string Expands, params string[] Using)
        {
            bool AlreadyExists = Data.TryGetValue(RepositoryHash, out RepositorySignature Repository);

            if (AlreadyExists) return false;

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

