using System.Text;
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
    private const string StringPattern = "(X|B|BX|N|NX)*(\"|\')(.*?)(\"|\'|$)";
    private const string WordsPattern = @"|[a-zA-Z]+([-|_]*[\w0-9]+)*|[0-9]+([-|_][\w0-9]+)+";
    private const string NumberPattern = @"|(\+|-)?\.?[0-9]\d*(\.\d+)?";
    private const string DirectivesPattern = "|^.*(>>[A-Z]*(-[A-Z0-9]*)*).*$";
    private const string SymbolPattern = @"|(\+|\-|\*\*|\*|=|\/|\$|,|;|::|\.|\(|\)|>>|<>|>=|<=|>|<|&|_)";
    private const string AllPatterns = StringPattern + WordsPattern + NumberPattern + DirectivesPattern + SymbolPattern;

    public static void TokenizeLine(List<Token> sourceTokens, ReadOnlySpan<byte> bytes, int lineNumber)
    {
        var charCount = Encoding.UTF8.GetCharCount(bytes);
        var maxStackLimit = 256;

        Span<char> sourceChars = charCount <= maxStackLimit 
            ? stackalloc char[charCount]
            : new char[charCount];

        Preprocessor.PreprocessSourceFormat(bytes, sourceChars);

        foreach (var token in LexerRegex().EnumerateMatches(sourceChars))
        {
            ReadOnlySpan<char> currentMatch = sourceChars.Slice(token.Index, token.Length);

            if (currentMatch.Contains(">>", StringComparison.OrdinalIgnoreCase))
            {
                Preprocessor.PreprocessDirective(currentMatch, lineNumber);
                continue;
            }

            Token tokenized = new(new string(currentMatch), lineNumber, token.Index + 1);
            sourceTokens.Add(tokenized);
        }
    }

    [GeneratedRegex(AllPatterns, RegexOptions.ExplicitCapture | RegexOptions.NonBacktracking | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant)]
    private static partial Regex LexerRegex();
}
