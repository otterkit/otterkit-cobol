using System.Text.RegularExpressions;

namespace Otterkit;

enum DirectiveType
{
    SourceFormat,
    None,
}

public static class Preprocessor
{
    public static Options Options = OtterkitCompiler.Options;
    public static List<string> SourceLines = new();
    public static int Index = 0;

    public static List<string> Preprocess(List<string> sourceLines)
    {
        List<string> preprocessedLines = new();
        SourceLines = sourceLines;

        DirectiveType lastDirective = DirectiveType.None;

        while (Index <= SourceLines.Count - 1)
        {
            string currentLine = SourceLines[Index];
            (currentLine, lastDirective) = PreprocessDirectives(currentLine);

            if (Options.SourceFormat == "fixed")
            {
                if (currentLine.Length >= Options.ColumnLength)
                {
                    // Removes everything after the max column length
                    currentLine = currentLine.Substring(0, Options.ColumnLength);
                }

                // Removes the sequence number area
                currentLine = currentLine.PadRight(7).Substring(6);

                if (currentLine.StartsWith("*"))
                {
                    // Removes all fixed format comment lines
                    currentLine = "";
                }

                currentLine = currentLine.Substring(1);

                preprocessedLines.Add(currentLine);
            }

            if (Options.SourceFormat == "free")
            {
                int commentIndex = currentLine.IndexOf("*>");
                if (commentIndex > -1)
                {
                    // Removes all free format comments
                    currentLine = currentLine.Substring(0, commentIndex);
                }

                preprocessedLines.Add(currentLine);
            }

            NextLine();
        }

        return preprocessedLines;
    }

    private static (string, DirectiveType) PreprocessDirectives(string currentLine)
    {
        int index = 0;
        List<Token> lexedLine = new();
        string directivePattern = """
        ^\s*>>SOURCE
        """;

        if (!Regex.IsMatch(currentLine, directivePattern, RegexOptions.IgnoreCase | RegexOptions.ExplicitCapture))
            return (currentLine, DirectiveType.None);
        
        lexedLine = Lexer.TokenizeLine(currentLine);

        Continue();
        if (CurrentEquals("SOURCE"))
        {
            Continue();
            if (CurrentEquals("FORMAT"))
                Continue();

            if (CurrentEquals("IS"))
                Continue();

            if (CurrentEquals("FREE"))
            {
                Options.SourceFormat = "free";
                return ("", DirectiveType.SourceFormat);
            }

            if (CurrentEquals("FIXED"))
            {
                Options.SourceFormat = "fixed";
                return ("", DirectiveType.SourceFormat);
            }
        }

        return (currentLine, DirectiveType.None);

        // Token Lookahead(int amount)
        // {
        //     return lexedLine[Index + amount];
        // }

        // bool LookaheadEquals(int lookahead, string stringToCompare)
        // {
        //     return Lookahead(lookahead).value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
        // }

        Token Current()
        {
            return lexedLine[index];
        }

        bool CurrentEquals(string stringToCompare)
        {
            return Current().value.Equals(stringToCompare, StringComparison.OrdinalIgnoreCase);
        }

        void Continue()
        {
            index += 1;
            return;
        }
    }

    private static void NextLine()
    {
        Index = Index + 1;
    }
}