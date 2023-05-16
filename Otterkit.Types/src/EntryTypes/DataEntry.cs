using static Otterkit.Types.TokenHandling;

namespace Otterkit.Types;

public partial class DataEntry : AbstractEntry
{
    public Option<DataEntry> Parent;
    public Option<string> ExternalizedName;

    public SourceScope Section;
    public int LevelNumber;

    public Classes Class;
    public Categories Category;
    public Usages Usage;

    public bool IsGroup;
    public bool IsConstant;

    private ulong ClauseBitField;
    public int ClauseDeclaration;

    public DataEntry(Token identifier, EntryKind entryKind)
        : base (identifier, entryKind) { }

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

    public bool FetchTypedef()
    {
        var storedIndex = SetupClauseFetch(DataClause.Type);

        var isStrong = false;

        while (CurrentEquals(TokenContext.IsClause))
        {
            if (CurrentEquals("TYPEDEF") && PeekEquals(1, "STRONG"))
            {
                isStrong = true;
                break;
            }

            Continue();
        }

        TokenHandling.Index = storedIndex;

        return isStrong;
    }

    public Token FetchType()
    {
        var storedIndex = SetupClauseFetch(DataClause.Type);

        while (CurrentEquals(TokenContext.IsClause))
        {
            if (CurrentEquals("TYPE") && PeekEquals(1, TokenType.Identifier))
            {
                Continue();
                break;
            }

            Continue();
        }

        var type = Current();

        TokenHandling.Index = storedIndex;

        return type;
    }

    public Token FetchGroupUsage()
    {
        var storedIndex = SetupClauseFetch(DataClause.GroupUsage);

        while (CurrentEquals(TokenContext.IsClause))
        {
            if (CurrentEquals("GROUP-USAGE"))
            {
                Continue();
                Optional("IS");
                break;
            }

            Continue();
        }

        var groupUsage = Current();

        TokenHandling.Index = storedIndex;

        return groupUsage;
    }

    public (Option<Token>, bool, bool) FetchObjectReference()
    {
        var storedIndex = SetupClauseFetch(DataClause.Usage);

        Option<Token> objectType = new();
        var isFactory = false;
        var isOnly = false;

        while (CurrentEquals(TokenContext.IsClause))
        {
            if (CurrentEquals("USAGE"))
            {
                Continue();
                Optional("IS");
                Continue();
                Continue();

                if (CurrentEquals("FACTORY"))
                {
                    Continue();
                    Optional("OF");
                    isFactory = true;
                }

                if (CurrentEquals(TokenType.Identifier))
                {
                    objectType = Current();
                    Continue();
                }

                if (CurrentEquals("ONLY"))
                {
                    isOnly = true;
                }

                break;
            }

            Continue();
        }

        TokenHandling.Index = storedIndex;

        return (objectType, isFactory, isOnly);
    }

    public Token FetchPicture()
    {
        var storedIndex = SetupClauseFetch(DataClause.Picture);

        while (CurrentEquals(TokenContext.IsClause))
        {
            if (CurrentEquals("PICTURE PIC"))
            {
                Continue();
                Optional("IS");
                break;
            }

            Continue();
        }

        var picture = Current();

        TokenHandling.Index = storedIndex;

        return picture;
    }

    private int SetupClauseFetch(DataClause clauseType)
    {
        if (!this[clauseType])
        {
            throw new NullReferenceException("NOTE: Always check if clause is present before running this method.");
        }

        var currentIndex = TokenHandling.Index;

        TokenHandling.Index = ClauseDeclaration;

        return currentIndex;
    }
}
