namespace Otterkit.Types;

public partial class DataEntry : AbstractEntry
{
    // TYPEDEF CLAUSE CONTEXT
    public bool TypedefStrong;

    // TYPE CLAUSE CONTEXT
    public Option<Token> Type;

    // PICTURE CLAUSE CONTEXT
    public int PictureLength;
    public Option<Token> PictureString;

    // VALUE CLAUSE CONTEXT
    public Option<Token> DefaultValue;

    // USAGE CLAUSE CONTEXT
    public UsageType UsageType;
    public Option<Token> UsageContext;

}