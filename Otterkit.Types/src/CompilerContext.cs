namespace Otterkit.Types;

public static class CompilerContext
{
    /// <summary>
    /// Used for storing the file names of all current compilation units.
    /// It also includes the names of all copybook files.
    /// </summary>
    public static readonly List<string> FileNames = new();
    
    /// <summary>
    /// Used for storing the tokens of all current compilation units.
    /// Each of them is separated by an EOF token.
    /// </summary>
    public static readonly List<Token> SourceTokens = new();

    /// <summary>
    /// Used for keeping track of which scope is being parsed.
    /// Scope meaning the current division, section or paragragh.
    /// </summary>
    public static CurrentScope ActiveScope { get; set; }

    /// <summary>
    /// Used for keeping track of where the current source unit was defined, including its containing parent.
    /// We store the token itself, because it already stores where it was written.
    /// </summary>
    public static readonly Stack<Token> ActiveUnits = new();

    /// <summary>
    /// Used for keeping track of the source unit types, including the type of its containing parent.
    /// </summary>
    public static readonly Stack<SourceUnit> SourceTypes = new();

    /// <summary>
    /// Used for storing the current source unit signature.
    /// </summary>
    private static Option<CallablePrototype> StoredSignature;

    /// <summary>
    /// Used for getting and setting the signature of the source unit currently being parsed.
    /// </summary>
    public static CallablePrototype ActiveSignature
    {
        get => (CallablePrototype)StoredSignature;

        set => StoredSignature = value;
    }

    public static LocalEntries<DataEntry> ActiveEntries
    {
        get => ((CallablePrototype)StoredSignature).DataEntries;
    }

    public static bool IsResolutionPass { get; set; }
}
