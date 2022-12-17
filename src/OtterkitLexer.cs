using System.Text.RegularExpressions;

namespace Otterkit;
public static class Lexer
{
    public static List<Token> Tokenize(List<string> sourceLines)
    {
        /*  
        **  Explaining the big regex patterns:
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

        string wordsPattern =  @"[a-zA-Z]+([-|_]*[\w0-9]+)*|[0-9]+([-|_][\w0-9]+)+";
        string stringPattern = "|(\")(.*?)(\"|$)|(\')(.*?)(\'|$)";
        string numberPattern = @"|(\+|-)?\.?[0-9]\d*(\.\d+)?";
        string symbolPattern = @"|(\+|\-|\*\*|\*|=|\/|\$|,|;|::|\.|\(|\)|>>|<>|>=|<=|>|<|&|_)";
        string allPatterns = wordsPattern + stringPattern + numberPattern + symbolPattern;

        int lineNumber = 0;
        List<Token> tokens = new();
        foreach (string line in sourceLines)
        {
            lineNumber += 1;
            foreach (Match token in Regex.Matches(line, allPatterns, 
            RegexOptions.IgnoreCase
            | RegexOptions.ExplicitCapture
            | RegexOptions.Compiled
            | RegexOptions.NonBacktracking
            ))
            {
                Token tokenized = new Token(token.Value, lineNumber, token.Index);
                tokens.Add(tokenized);
            }
        }
        return tokens;
    }

    public static List<Token> TokenizeLine(string sourceLine)
    {
        List<string> sourceLines = new();
        sourceLines.Add(sourceLine);

        return Tokenize(sourceLines);
    }
}