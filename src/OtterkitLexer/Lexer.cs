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

    private static bool IsPictureNext;

    public static void TokenizeLine(List<Token> sourceTokens, ReadOnlySpan<byte> bytes, int lineNumber)
    {
        var charCount = Encoding.UTF8.GetCharCount(bytes);
        var maxStackLimit = 256;

        Span<char> sourceChars = charCount <= maxStackLimit 
            ? stackalloc char[charCount]
            : new char[charCount];

        Preprocessor.PreprocessSourceFormat(bytes, sourceChars);

        var PictureEndIndex = 0;

        foreach (var token in LexerRegex().EnumerateMatches(sourceChars))
        {
            ReadOnlySpan<char> currentMatch = sourceChars.Slice(token.Index, token.Length);

            if (token.Index < PictureEndIndex) continue;

            if (currentMatch.Contains(">>", StringComparison.OrdinalIgnoreCase))
            {
                Preprocessor.PreprocessDirective(currentMatch, lineNumber);
                continue;
            }

            if (IsPictureNext && !currentMatch.Equals("IS", StringComparison.OrdinalIgnoreCase))
            {
                var temporary = sourceChars.Slice(token.Index, sourceChars.Length - token.Index - 1);

                var regex = PictureEndRegex().EnumerateMatches(temporary);

                regex.MoveNext();

                var pictureChars = temporary.Slice(0, regex.Current.Index);

                Token picture = new(new string(pictureChars), TokenType.Picture, lineNumber, token.Index + 1);
                sourceTokens.Add(picture);

                PictureEndIndex = regex.Current.Index + token.Index;
                IsPictureNext = false;

                continue;
            }

            IsPictureNext = currentMatch[0] is 'P' or 'p' 
                && (currentMatch.Equals("PIC", StringComparison.OrdinalIgnoreCase)
                || currentMatch.Equals("PICTURE", StringComparison.OrdinalIgnoreCase));

            TokenType type = currentMatch switch
            {
                ['"' or '\'', .. _, '"' or '\'',] => TokenType.String,
                ['X' or 'x', '"' or '\'', .. _, '"' or '\'',] => TokenType.HexString,
                ['B' or 'b', '"' or '\'', .. _, '"' or '\'',] => TokenType.Boolean,
                ['B' or 'B', 'X' or 'x', '"' or '\'', .. _, '"' or '\'',] => TokenType.HexBoolean,
                ['N' or 'n', '"' or '\'', .. _, '"' or '\'',] => TokenType.National,
                ['N' or 'n', 'X' or 'x', '"' or '\'', .. _, '"' or '\'',] => TokenType.HexNational,
                [..] => TokenType.None
            };

            if (type is not TokenType.None)
            {
                PreprocessStringLiteral(type, currentMatch, out var preprocessed);

                Token stringLiteral = new(new string(preprocessed), type, lineNumber, token.Index + 1);
                sourceTokens.Add(stringLiteral);

                continue;
            }

            Token tokenized = new(new string(currentMatch), type, lineNumber, token.Index + 1);
            sourceTokens.Add(tokenized);
        }
    }

    private static void PreprocessStringLiteral(TokenType type, ReadOnlySpan<char> chars, out ReadOnlySpan<char> preprocessed)
    {
        if (type is TokenType.String)
        {
            preprocessed = chars.Slice(1, chars.Length - 2);
            return;
        }

        if (type is TokenType.HexString)
        {
            preprocessed = chars.Slice(2, chars.Length - 3);
            return;
        }

        if (type is TokenType.Boolean)
        {
            preprocessed = chars.Slice(2, chars.Length - 3);
            return;
        }

        if (type is TokenType.HexBoolean)
        {
            preprocessed = chars.Slice(3, chars.Length - 4);
            return;
        }

        if (type is TokenType.National)
        {
            preprocessed = chars.Slice(2, chars.Length - 3);
            return;
        }

        if (type is TokenType.HexNational)
        {
            preprocessed = chars.Slice(3, chars.Length - 4);
            return;
        }

        preprocessed = new ReadOnlySpan<char>();
    }

    [GeneratedRegex(AllPatterns, RegexOptions.ExplicitCapture | RegexOptions.NonBacktracking | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant)]
    private static partial Regex LexerRegex();

    [GeneratedRegex("""(\s|\.\s)""", RegexOptions.ExplicitCapture | RegexOptions.NonBacktracking | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant)]
    private static partial Regex PictureEndRegex();
}
