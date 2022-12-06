using System.Diagnostics;

namespace Otterkit;

public struct DataItems
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
    public static Dictionary<string, DataItems> Data = new();

    public static bool AddDataItem(string DataItemHash, string Identifier, int LevelNumber)
    {
        DataItems DataItem;
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
        DataItems DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.Type = Type;

        return false;
    }

    public static bool AddPicture(string DataItemHash, string Picture)
    {
        DataItems DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.PictureLength = Picture;

        return false;
    }

    public static bool AddDefault(string DataItemHash, string Default)
    {
        DataItems DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.DefaultValue = Default;

        return false;
    }

    public static bool IsExternal(string DataItemHash, bool IsExternal, string ExternalName)
    {
        DataItems DataItem;
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
        DataItems DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.IsElementary = IsElementary;

        return false;
    }

    public static bool IsGroup(string DataItemHash, bool IsGroup)
    {
        DataItems DataItem;
        bool AlreadyExists = Data.TryGetValue(DataItemHash, out DataItem);

        if (AlreadyExists)
            DataItem.IsGroup = IsGroup;

        return false;
    }
}