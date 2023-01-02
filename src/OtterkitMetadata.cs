using System.Diagnostics;

namespace Otterkit;

public struct DataItemInfo
{
    public string Section;
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

public static class DataItemInformation
{
    public static Dictionary<string, DataItemInfo> Data = new();

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

    public static bool AddSection(string DataItemHash, string Section)
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