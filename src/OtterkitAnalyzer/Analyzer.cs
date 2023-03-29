namespace Otterkit;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static partial class Analyzer
{
    private static string FileName = string.Empty;
    private static bool IsResolutionPass;
    private static CurrentScope CurrentSection;
    private static readonly Stack<Token> CurrentId = new();
    private static readonly Stack<SourceUnit> SourceType = new();
    private static CallableSignature CurrentSourceUnit
    {
        get => CompilerContext.CurrentCallable[0];

        set => CompilerContext.CurrentCallable[0] = value;
    }

    /// <summary>
    /// Otterkit COBOL Syntax Analyzer
    /// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
    /// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
    /// </summary>
    public static List<Token> Analyze(string entryPoint)
    {
        FileName = entryPoint;

        // Call the parser's main recursive method
        // This should only return when the parser reaches the true EOF token
        Source();

        // If a parsing error has occured, terminate the compilation process.
        // We do not want the compiler to continue when the source code is not valid.
        if (ErrorHandler.HasError) ErrorHandler.Terminate("parsing");

        // Return parsed list of tokens.
        return CompilerContext.SourceTokens;
    }

    // Source() is the main method of the parser.
    // It's responsible for parsing COBOL divisions until the EOF token.
    // If EOF was not returned as the last Token in the list then,
    // the parser has not finished reading through the list of tokens correctly.
    public static void Source()
    {
        IDENTIFICATION();

        if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

        if (CurrentEquals("DATA")) DATA();

        bool notClassOrInterface = SourceType.Peek() switch
        {
            SourceUnit.Class => false,
            SourceUnit.Interface => false,
            _ => true
        };

        if (notClassOrInterface)
        {
            if (CurrentEquals("PROCEDURE")) 
                PROCEDURE();
        }
        else if (SourceType.Peek() == SourceUnit.Class)
        {
            ClassObjects();
        }
        else if (SourceType.Peek() == SourceUnit.Interface)
        {
            InterfaceProcedure();
        }

        EndMarker();

        if (CurrentEquals("IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID"))
        {
            Source();
        }

        if (CurrentEquals("EOF") && CurrentIndex() < CompilerContext.SourceTokens.Count - 1)
        {
            FileName = Lookahead(1).FetchFile;

            Continue();
            Source();
        }
    }
}
