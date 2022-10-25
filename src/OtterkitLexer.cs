using System.Text.RegularExpressions;

namespace Otterkit;
public static class OtterkitLexer
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
        */

        int lineNumber = 0;
        string wordsPattern =  @"[a-zA-Z]+([-|_]*[\w0-9]+)*|[0-9]+([-|_][\w0-9]+)+|\w+";
        string stringPattern = "|(\")(.*?)(\"|$)|(\')(.*?)(\'|$)";
        string numberPattern = @"|(\+|-)?\.?[0-9]\d*(\.\d+)?";
        string symbolPattern = @"|(\+|\-|\*\*|\*|=|\/|\$|,|;|::|\.|\(|\)|>>|<>|>=|<=|>|<|&|_)";
        string allPatterns = wordsPattern + stringPattern + numberPattern + symbolPattern;
        List<Token> tokens = new();
        foreach (string line in sourceLines)
        {
            lineNumber += 1;
            foreach (Match token in Regex.Matches(line, allPatterns, RegexOptions.IgnoreCase))
            {
                Token tokenized = new(token.Value, "", "", lineNumber, token.Index);
                tokens.Add(tokenized);
            }
        }
        return tokens;
    }
}