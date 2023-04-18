namespace Otterkit.Types;

public partial class DataEntry : AbstractEntry
{
    public int LevelNumber;
    public DataEntry? Parent;
    public CurrentScope Section;
    private Compact<bool> HasClause;

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
