namespace Otterkit.Types;

public partial class DataEntry
{
    public Option<DataEntry> Parent;
    public Option<Token> Identifier;
    public string ExternalizedName = "";
    public EntryType EntryType;

    public CurrentScope Section;
    public UsageType Usage;
    public int LevelNumber;

    public bool IsElementary;
    public bool IsConstant;
    public bool IsGroup;

    private ulong ClauseBitField;

    public DataEntry(Token identifier, EntryType entryType) 
    {
        Identifier = identifier;
        EntryType = entryType;
    }

    public bool this[DataClause clauseName]
    {
        get => GetClauseBit(clauseName);

        set => SetClauseBit(clauseName, value);
    }

    private void SetClauseBit(DataClause clause, bool bit)
    {
        var mask = 1UL << (int)clause - 1;

        if (bit)
        {
            ClauseBitField |= mask;
            return;
        }

        ClauseBitField &= ~mask;
    }

    private bool GetClauseBit(DataClause clause)
    {
        var position = (int)clause - 1;

        var bit = (ClauseBitField >> position) & 1;

        return bit == 1UL;
    }
}
