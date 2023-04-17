namespace Otterkit.Types;

public class DataEntry : AbstractEntry
{
    public DataEntry? Parent;
    public CurrentScope Section;
    public Compact<bool> HasClause;
    public int LevelNumber;
    public string PictureString = "";
    public int PictureLength;
    public string Type = "";
    public string DefaultValue = "";
    public UsageType UsageType;
    public string UsageContext = "";
    public bool IsElementary;
    public bool IsGroup;
    public bool IsConstant;

    public DataEntry(Token identifier, EntryType entryType) 
        : base(identifier, entryType)
    {
        HasClause = new Compact<bool>(65);
    }

    public bool this[DataClause clauseName]
    {
        get => HasClause[(int)clauseName];

        set => HasClause[(int)clauseName] = value;
    }

    public bool this[int clauseIndex]
    {
        get => HasClause[clauseIndex];

        set => HasClause[clauseIndex] = value;
    }
}
