using System.Text;
using System.Text.RegularExpressions;
using Otterkit.Types;

namespace Otterkit.Tokenizers;

public static partial class Tokenizer
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
    private const string StringPattern = "(X|B|BX|N|NX)*(\"|\')(.*?)(\"-?|\'-?|$)";
    private const string WordsPattern = @"|[a-zA-Z]+([-|_]*[\w0-9]+)*|[0-9]+([-|_][\w0-9]+)+";
    private const string NumberPattern = @"|(\+|-)?\.?[0-9]\d*(\.\d+)?";
    private const string DirectivesPattern = "|^.*(>>[A-Z]*(-[A-Z0-9]*)*).*$";
    private const string SymbolPattern = @"|(\+|\-|\*\*|\*|=|\/|\$|,|;|::|\.|\(|\)|>>|<>|>=|<=|>|<|&|_)";
    private const string AllPatterns = StringPattern + WordsPattern + NumberPattern + DirectivesPattern + SymbolPattern;

    private static bool IsPictureNext;

    private static void TokenizeLine(List<Token> sourceTokens, ReadOnlySpan<byte> bytes, int lineIndex)
    {
        var charCount = Encoding.UTF8.GetCharCount(bytes);
        var maxStackLimit = 256;

        Span<char> sourceChars = charCount <= maxStackLimit 
            ? stackalloc char[charCount]
            : new char[charCount];

        PreprocessSourceFormat(bytes, sourceChars);

        var pictureEndIndex = 0;

        var fileIndex = CompilerContext.FileNames.Count - 1;

        foreach (var token in LexerRegex().EnumerateMatches(sourceChars))
        {
            ReadOnlySpan<char> current = sourceChars.Slice(token.Index, token.Length);

            if (token.Index < pictureEndIndex) continue;

            if (current.Contains(">>", StringComparison.OrdinalIgnoreCase))
            {
                PreprocessDirective(current, lineIndex);
                continue;
            }

            if (IsPictureNext && !current.Equals("IS", StringComparison.OrdinalIgnoreCase))
            {
                var temporary = sourceChars.Slice(token.Index, sourceChars.Length - token.Index);

                var regex = PictureEndRegex().EnumerateMatches(temporary);

                regex.MoveNext();

                var matchIndex = regex.Current.Index;

                var pictureChars = temporary.Slice(0, matchIndex);

                if (matchIndex is 0)
                {
                    pictureChars = temporary;
                }

                Token picture = new(new string(pictureChars), TokenType.Picture)
                {
                    Line = lineIndex,
                    Column = token.Index + 1,
                    FileIndex = fileIndex
                };

                sourceTokens.Add(picture);

                pictureEndIndex = pictureChars.Length + token.Index;

                IsPictureNext = false;

                continue;
            }

            IsPictureNext = IsPictureNext || current[0] is 'P' or 'p' && (CurrentEquals(current, "PIC") || CurrentEquals(current, "PICTURE"));

            TokenType type = current switch
            {
                ['"' or '\'', .. _, '"' or '\'', '-'] or
                ['"' or '\'', .. _, '"' or '\''] or
                ['"' or '\'', .. _] => TokenType.String,

                ['X' or 'x', '"' or '\'', .. _, '"' or '\'', '-'] or
                ['X' or 'x', '"' or '\'', .. _, '"' or '\''] or
                ['X' or 'x', '"' or '\'', .. _] => TokenType.HexString,

                ['B' or 'b', '"' or '\'', .. _, '"' or '\'', '-'] or
                ['B' or 'b', '"' or '\'', .. _, '"' or '\''] or
                ['B' or 'b', '"' or '\'', .. _] => TokenType.Boolean,

                ['B' or 'B', 'X' or 'x', '"' or '\'', .. _, '"' or '\'', '-'] or
                ['B' or 'B', 'X' or 'x', '"' or '\'', .. _, '"' or '\''] or
                ['B' or 'B', 'X' or 'x', '"' or '\'', .. _] => TokenType.HexBoolean,

                ['N' or 'n', '"' or '\'', .. _, '"' or '\'',] => TokenType.National,
                
                ['N' or 'n', 'X' or 'x', '"' or '\'', .. _, '"' or '\'',] => TokenType.HexNational,

                [..] => TokenType.None
            };

            if (type is not TokenType.None)
            {
                Token stringLiteral = new(new string(current), type)
                {
                    Line = lineIndex,
                    Column = token.Index + 1,
                    FileIndex = fileIndex
                };

                sourceTokens.Add(stringLiteral);

                continue;
            }

            Token tokenized = new(new string(current), type)
            {
                Line = lineIndex,
                Column = token.Index + 1,
                FileIndex = fileIndex
            };

            sourceTokens.Add(tokenized);
        }
    }

    private static bool CurrentEquals(ReadOnlySpan<char> match, ReadOnlySpan<char> value)
    {
        return match.Equals(value, StringComparison.OrdinalIgnoreCase);
    }

    [GeneratedRegex(AllPatterns, RegexOptions.ExplicitCapture | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant)]
    private static partial Regex LexerRegex();

    [GeneratedRegex("""(\s|\.\s)""", RegexOptions.ExplicitCapture | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant)]
    private static partial Regex PictureEndRegex();
}
