namespace Otterkit;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This analyzer was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a <see cref="CompilerContext.SourceTokens">list of tokens</see> generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static partial class Analyzer
{
    private static bool IsResolutionPass;

    /// <summary>
    /// Used for keeping track of the current scope 
    /// (scope meaning the current division, section or paragragh).
    /// </summary>
    private static CurrentScope CurrentScope;

    /// <summary>
    /// Used for keeping track of where the source unit was defined, including its containing parent.
    /// </summary>
    private static readonly Stack<Token> CurrentId = new();

    /// <summary>
    /// Used for keeping track of the source unit types, including the type of its containing parent.
    /// </summary>    
    private static readonly Stack<SourceUnit> SourceType = new();

    /// <summary>
    /// Used for keeping track of the current source unit signature.
    /// </summary>    
    private static CallableSignature CurrentCallable
    {
        get => CompilerContext.CurrentCallable[0];

        set => CompilerContext.CurrentCallable[0] = value;
    }

    /// <summary>
    /// Otterkit COBOL Syntax and Semantic Analyzer
    /// <para>This analyzer was built to be easily extensible, with some reusable COBOL parts.</para>
    /// <para>It requires a <see cref="CompilerContext.SourceTokens">list of tokens</see> generated from the Lexer and the Token Classifier.</para>
    /// </summary>
    public static List<Token> Analyze(List<Token> tokenList)
    {
        // Call the analyzer's main recursive method
        // This should only return when the analyzer reaches the true EOF token
        Source();

        // Reset token index and setup resolution pass
        SetupResolutionPass();

        // Call the analyzer's main recursive method again
        // But this time with name resolution enabled
        Source();

        // If a parsing error has occured, terminate the compilation process.
        // We do not want the compiler to continue when the source code is not valid.
        if (Error.HasOccurred) Error.StopCompilation("parsing");

        // Return parsed list of tokens.
        return tokenList;
    }

    // Source() is the main method of the analyzer.
    // It's responsible for parsing COBOL divisions until the EOF token.
    // If EOF was not returned as the last Token in the list then,
    // the analyzer has not finished reading through the list of tokens correctly.
    private static void Source()
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
            Continue();
            Source();
        }
    }

    private static void SetupResolutionPass()
    {
        // Set the index back to 0 to restart the analyzer
        Index = 0;

        // Enable name resolution checks
        IsResolutionPass = true;

        // Suppress analyzer error messages to avoid duplicates
        // Note: Resolution errors should use 'ErrorType.Resolution'
        Error.SuppressedError = ErrorType.Analyzer;
    }
}
