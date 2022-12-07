using System.Diagnostics;

namespace Otterkit;

public struct DataItemInfo
{
    public string Section;
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
}

public static class DataItemInformation
{
    public static Dictionary<string, DataItemInfo> Data = new();

    public static DataItemInfo GetValue(string DataItemHash)
    {
        DataItemInfo DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (!AlreadyExists)
            throw new ArgumentException("FAILED TO GET DATA ITEM HASH: THIS SHOULD NOT HAVE HAPPENED, PLEASE REPORT THIS ISSUE ON OTTERKIT'S REPO");
        
        return DataItem;
    }

    public static bool AddDataItem(string DataItemHash, string Identifier, int LevelNumber)
    {
        DataItemInfo DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            return false;

        DataItem.LevelNumber = LevelNumber;
        DataItem.Identifier = Identifier;
        Data.Add(DataItemHash, DataItem);
        return true;
    }

    public static bool AddType(string DataItemHash, string Type)
    {
        DataItemInfo DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.Type = Type;

        return false;
    }

    public static bool AddPicture(string DataItemHash, string Picture)
    {
        DataItemInfo DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.PictureLength = Picture;

        return false;
    }

    public static bool AddDefault(string DataItemHash, string Default)
    {
        DataItemInfo DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.DefaultValue = Default;

        return false;
    }

    public static bool IsExternal(string DataItemHash, bool IsExternal, string ExternalName)
    {
        DataItemInfo DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
        {
            DataItem.IsExternal = IsExternal;
            DataItem.ExternalName = ExternalName;
        }

        return false;
    }

    public static bool IsElementary(string DataItemHash, bool IsElementary)
    {
        DataItemInfo DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.IsElementary = IsElementary;

        return false;
    }

    public static bool IsGroup(string DataItemHash, bool IsGroup)
    {
        DataItemInfo DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.IsGroup = IsGroup;

        return false;
    }
}