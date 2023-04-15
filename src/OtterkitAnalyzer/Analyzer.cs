using static Otterkit.SourceAnalyzer.TokenHandling;

namespace Otterkit.SourceAnalyzer;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This analyzer was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a <see cref="CompilerContext.SourceTokens">list of tokens</see> generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static class Analyzer
{
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
        IdentificationDivision.Parse();

        var sourceTypes = CompilerContext.SourceTypes;

        if (CurrentEquals("ENVIRONMENT"))
        {
            EnvironmentDivision.Parse();
        }

        if (CurrentEquals("DATA"))
        {
            DataDivision.Parse();
        }

        var isClassOrInterface = sourceTypes.Peek() switch
        {
            SourceUnit.Class => true,
            SourceUnit.Interface => true,
            _ => false
        };

        if (!isClassOrInterface)
        {
            if (CurrentEquals("PROCEDURE")) 
            {
                ProcedureDivision.ParseProcedural();
            }
        }
        else if (sourceTypes.Peek() == SourceUnit.Class)
        {
            ProcedureDivision.ParseObjects();
        }
        else if (sourceTypes.Peek() == SourceUnit.Interface)
        {
            ProcedureDivision.ParseInterface();
        }

        ProcedureDivision.EndMarker();

        if (CurrentEquals("IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID"))
        {
            Source();
        }

        if (CurrentEquals("EOF") && TokenHandling.Index < CompilerContext.SourceTokens.Count - 1)
        {
            Continue();
            Source();
        }
    }

    private static void SetupResolutionPass()
    {
        // Set the index back to 0 to restart the analyzer
        TokenHandling.Index = 0;

        // Enable name resolution checks
        CompilerContext.IsResolutionPass = true;

        // Suppress analyzer error messages to avoid duplicates
        // Note: Resolution errors should use 'ErrorType.Resolution'
        Error.SuppressedError = ErrorType.Analyzer;
    }
}
