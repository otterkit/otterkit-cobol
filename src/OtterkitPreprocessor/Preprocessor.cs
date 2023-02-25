using System.Text.RegularExpressions;
using System.Text;

namespace Otterkit;

public static partial class Preprocessor
{
    internal static Options Options = OtterkitCompiler.Options;
    internal static DirectiveType lastDirective = DirectiveType.None;

    public static List<Token> Preprocess(string entryPoint)
    {
        if (!File.Exists(entryPoint))
        {
            ErrorHandler.Compiler.Report("Otterkit compiler error: File Not Found");
            ErrorHandler.Compiler.Report($"The compiler was not able not find the file: {entryPoint}");
            Environment.Exit(1);
        }

        var allSourceFiles = Directory.EnumerateFiles(Directory.GetCurrentDirectory(), "*.cob", SearchOption.AllDirectories);
        var excludeEntryPoint = Directory.EnumerateFiles(Directory.GetCurrentDirectory(), entryPoint);
        var sourcesWithoutEntryPoint = allSourceFiles.Except(excludeEntryPoint);

        List<Token> files = Preprocessor.ReadSourceFile(entryPoint).Result;

        foreach (var file in sourcesWithoutEntryPoint)
        {
            files = Preprocessor.ReadSourceFile(file).Result;
            
            Options.FileNames.Add(file);
        }

        PreprocessCopybooks(SourceTokens);

        return SourceTokens;
    }

    public static void PreprocessSourceFormat(ReadOnlySpan<byte> bytes, Span<char> chars)
    {
        var charCount = Encoding.UTF8.GetCharCount(bytes);
        var maxStackLimit = 256;

        Span<char> sourceChars = charCount <= maxStackLimit 
            ? stackalloc char[charCount]
            : new char[charCount];

        Encoding.UTF8.GetChars(bytes, sourceChars);

        if (Options.SourceFormat == "fixed")
        {
            if (sourceChars.Length >= Options.ColumnLength)
            {
                // Removes everything after the max column length
                sourceChars.Slice(Options.ColumnLength).Fill(' ');
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
                sourceChars = sourceChars.Slice(1);
            }
        }

        if (Options.SourceFormat == "free")
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

    public static void PreprocessDirective(ReadOnlySpan<char> directiveChars, int lineNumber)
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

            if (CurrentEquals("FORMAT")) Continue();

            if (CurrentEquals("IS")) Continue();

            if (CurrentEquals("FREE"))
            {
                Options.SourceFormat = "free";
            }

            if (CurrentEquals("FIXED"))
            {
                Options.SourceFormat = "fixed";
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

    public static void PreprocessCopybooks(List<Token> sourceTokens)
    {
        var index = 0;

        while (!(index >= sourceTokens.Count - 1))
        {
            if (!CurrentEquals("COPY"))
            {
                Continue();
                continue;
            }

            if (CurrentEquals("COPY"))
            {
                var copyIndex = index;
                Continue();

                var copybookTokens = ReadCopybook(Current().value).Result;

                Continue();

                var currentIndex = index;

                SourceTokens.RemoveRange(copyIndex, currentIndex - copyIndex);

                SourceTokens.InsertRange(copyIndex, copybookTokens);
                
            }
        }

        Token Current()
        {
            return sourceTokens[index];
        }

        bool CurrentEquals(string stringToCompare)
        {
            return Current().value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
        }

        void Continue()
        {
            if (index >= sourceTokens.Count - 1) return;

            index += 1;
        }
    }

    [GeneratedRegex("""(>>[A-Z]*(-[A-Z0-9]*)*)|[a-zA-Z]+([-|_]*[a-zA-Z0-9]+)*""", RegexOptions.ExplicitCapture | RegexOptions.NonBacktracking | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant)]
    private static partial Regex PreprocessorRegex();
}
