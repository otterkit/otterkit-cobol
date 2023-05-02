using System.Text.RegularExpressions;
using System.Text;
using Otterkit.Types;

namespace Otterkit.Tokenizers;

public static partial class Tokenizer
{
    private static void PreprocessSourceFormat(ReadOnlySpan<byte> bytes, Span<char> chars)
    {
        var charCount = Encoding.UTF8.GetCharCount(bytes);
        var maxStackLimit = 256;

        Span<char> sourceChars = charCount <= maxStackLimit 
            ? stackalloc char[charCount]
            : new char[charCount];

        Encoding.UTF8.GetChars(bytes, sourceChars);

        if (!HasDetectedSourceFormat && CompilerOptions.Format == SourceFormat.Auto)
        {
            if (sourceChars.Length >= 15 && sourceChars.Slice(7, 8).StartsWith(">>SOURCE"))
            {
                CompilerOptions.Format = SourceFormat.Fixed;
                HasDetectedSourceFormat = true;
            }

            if (sourceChars.Length >= 7 && sourceChars[6] is '*' or '-' or '/' or ' ')
            {
                CompilerOptions.Format = SourceFormat.Fixed;
                HasDetectedSourceFormat = true;
            }
            else
            {
                CompilerOptions.Format = SourceFormat.Free;
                HasDetectedSourceFormat = true;
            }

            if (sourceChars.Slice(0, 7).Trim().StartsWith("*>"))
            {
                CompilerOptions.Format = SourceFormat.Free;
                HasDetectedSourceFormat = true;
            }

            if (sourceChars.Slice(0, 7).Trim().StartsWith(">>"))
            {
                CompilerOptions.Format = SourceFormat.Free;
                HasDetectedSourceFormat = true;
            }

            if (sourceChars.Trim().Length == 0)
            {
                CompilerOptions.Format = SourceFormat.Auto;
                HasDetectedSourceFormat = false;
            }
        }

        if (CompilerOptions.Format == SourceFormat.Fixed || !HasDetectedSourceFormat)
        {
            if (sourceChars.Length >= CompilerOptions.Columns)
            {
                // Removes everything after the max column length
                sourceChars.Slice(CompilerOptions.Columns).Fill(' ');
            }

            // Removes the sequence number area
            if (sourceChars.Length >= 7)
            {
                sourceChars.Slice(0, 6).Fill(' ');
            }
            else
            {
                sourceChars.Fill(' ');
            }

            if (sourceChars.Length >= 7 && sourceChars[6].Equals('*'))
            {
                // Removes all fixed format comment lines
                sourceChars.Fill(' ');
            }

            int commentIndex = sourceChars.IndexOf("*>");
            if (commentIndex > -1)
            {
                // Removes all floating comments
                sourceChars = sourceChars.Slice(0, commentIndex);
            }

            if (sourceChars.Length >= 1)
            {
                sourceChars[0] = ' ';
            }
        }

        if (CompilerOptions.Format == SourceFormat.Free)
        {
            int commentIndex = sourceChars.IndexOf("*>");
            if (commentIndex > -1)
            {
                // Removes all floating comments
                sourceChars = sourceChars.Slice(0, commentIndex);
            }
        }

        sourceChars.CopyTo(chars);
    }

    private static void PreprocessDirective(ReadOnlySpan<char> directiveChars, int lineNumber)
    {
        List<DirectiveToken> directiveTokens = new();
        var index = 0;

        foreach (var token in PreprocessorRegex().EnumerateMatches(directiveChars))
        {
            ReadOnlySpan<char> currentMatch = directiveChars.Slice(token.Index, token.Length);

            DirectiveToken tokenized = new(new string(currentMatch), lineNumber);
            directiveTokens.Add(tokenized);
        }
        
        if (CurrentEquals(">>SOURCE"))
        {
            Continue();

            LastDirective = DirectiveType.SourceFormat;

            if (CurrentEquals("FORMAT")) Continue();

            if (CurrentEquals("IS")) Continue();

            if (CurrentEquals("FREE"))
            {
                CompilerOptions.Format = SourceFormat.Free;
            }

            if (CurrentEquals("FIXED"))
            {
                CompilerOptions.Format = SourceFormat.Fixed;
            }
        }

        DirectiveToken Current()
        {
            return directiveTokens[index];
        }

        bool CurrentEquals(string stringToCompare)
        {
            return Current().value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
        }

        void Continue()
        {
            if (index >= directiveTokens.Count - 1) return;

            index += 1;
        }
    }

    private static void PreprocessCopybooks(List<Token> sourceTokens)
    {
        var tokenIndex = 0;

        while (!(tokenIndex >= sourceTokens.Count - 1))
        {
            if (!CurrentEquals("COPY"))
            {
                Continue();
                continue;
            }

            if (CurrentEquals("COPY"))
            {
                var statementIndex = tokenIndex;

                Continue();

                var copybookName = Current().Value;

                CompilerContext.FileNames.Add(copybookName);

                var copybookTokens = ReadCopybook(copybookName).Result;

                Continue();

                var currentIndex = tokenIndex;

                CompilerContext.SourceTokens.RemoveRange(statementIndex, currentIndex - statementIndex);

                CompilerContext.SourceTokens.InsertRange(statementIndex, copybookTokens);
                
            }
        }

        Token Current()
        {
            return sourceTokens[tokenIndex];
        }

        bool CurrentEquals(string stringToCompare)
        {
            return Current().Value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
        }

        void Continue()
        {
            if (tokenIndex >= sourceTokens.Count - 1) return;

            tokenIndex += 1;
        }
    }

    [GeneratedRegex("""(>>[A-Z]*(-[A-Z0-9]*)*)|[a-zA-Z]+([-|_]*[a-zA-Z0-9]+)*""", RegexOptions.ExplicitCapture | RegexOptions.NonBacktracking | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant)]
    private static partial Regex PreprocessorRegex();
}
