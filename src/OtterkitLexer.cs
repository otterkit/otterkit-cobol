using System.Text.RegularExpressions;

namespace Otterkit;
public static partial class Lexer
{
    /*  
    **  Explaining the big regex pattern:
    **
    **  < wordsPattern > : Matches all COBOL reserved keywords,
    **                     intrinsic function names and identifiers
    **
    **  < stringPattern > :
    **          Matches all strings including fixed format continuation lines,
    **          where the string ends without closing the quote and continues
    **          with a -" on the next line. Useful for preserving string formatting
    **   
    **  < symbolPattern > : Matches all COBOL special characters and their valid
    **                      combinations
    **   
    **  < numberPattern > : Matches signed and unsigned number literals, including
    **                      decimal numbers that might start with 0. or just .
    */
    private const string WordsPattern = @"[a-zA-Z]+([-|_]*[\w0-9]+)*|[0-9]+([-|_][\w0-9]+)+";
    private const string StringPattern = "|(\")(.*?)(\"|$)|(\')(.*?)(\'|$)";
    private const string SymbolPattern = @"|(\+|\-|\*\*|\*|=|\/|\$|,|;| :: |\.|\(|\)|>>|<>|>=|<=|>|<|&|_)";
    private const string NumberPattern = @"|(\+|-)?\.?[0-9]\d*(\.\d+)?";
    private const string AllPatterns = WordsPattern + StringPattern + NumberPattern + SymbolPattern;

    public static List<Token> Tokenize(List<string> sourceLines)
    {
        int lineNumber = 0;
        List<Token> tokens = new();
        foreach (string line in sourceLines)
        {
            lineNumber += 1;
            foreach (Match token in LexerRegex().Matches(line).Cast<Match>())
            {
                Token tokenized = new(token.Value, lineNumber, token.Index);
                tokens.Add(tokenized);
            }
        }
        return tokens;
    }

    public static List<Token> TokenizeLine(string sourceLine)
    {
        List<string> sourceLines = new() { sourceLine };

        return Tokenize(sourceLines);
    }

    [GeneratedRegex(AllPatterns, RegexOptions.ExplicitCapture | RegexOptions.NonBacktracking | RegexOptions.IgnoreCase)]
    private static partial Regex LexerRegex();
}