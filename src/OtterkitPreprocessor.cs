using System.Text.RegularExpressions;
using System.Text;

namespace Otterkit;

public static partial class Preprocessor
{
    internal static Options Options = OtterkitCompiler.Options;
    internal static DirectiveType lastDirective = DirectiveType.None;
    internal static List<string> SourceLines = new();
    internal static int Index = 0;

    public static List<string> Preprocess(List<string> sourceLines)
    {
        List<string> preprocessedLines = new();
        SourceLines = sourceLines;

        while (Index <= SourceLines.Count - 1)
        {
            StringBuilder currentLine = new(PreprocessDirectives(SourceLines[Index]));

            if (Options.SourceFormat == "fixed")
            {
                if (currentLine.Length >= Options.ColumnLength)
                {
                    // Removes everything after the max column length
                    currentLine.Remove(Options.ColumnLength, currentLine.Length);
                }

                // Removes the sequence number area
                if (currentLine.Length >= 7)
                {
                    currentLine.Remove(0, 6);
                }
                else
                {
                    currentLine.Remove(0, currentLine.Length);
                }


                if (currentLine[0].Equals('*'))
                {
                    // Removes all fixed format comment lines
                    currentLine.Clear();
                }

                if (currentLine.Length >= 1)
                {
                    currentLine = currentLine.Remove(0, 1).Insert(0, "       ");
                    preprocessedLines.Add(currentLine.ToString());
                }
            }

            if (Options.SourceFormat == "free")
            {
                int commentIndex = currentLine.ToString().IndexOf("*>");
                if (commentIndex > -1)
                {
                    // Removes all free format comments
                    currentLine = currentLine.Remove(commentIndex, currentLine.Length);
                }

                preprocessedLines.Add(currentLine.ToString());
            }

            NextLine();
        }

        return preprocessedLines;
    }

    private static string PreprocessDirectives(string currentLine)
    {
        int index = 0;
        if (!PreprocessorRegex().IsMatch(currentLine))
            return currentLine;
        
        List<Token> lexedLine = Lexer.TokenizeLine(currentLine);

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
                return "";
            }

            if (CurrentEquals("FIXED"))
            {
                Options.SourceFormat = "fixed";
                return "";
            }
        }

        return currentLine;

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
        Index++;
    }

    [GeneratedRegex("""^\s*>>SOURCE""", RegexOptions.ExplicitCapture | RegexOptions.NonBacktracking | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant)]
    private static partial Regex PreprocessorRegex();
}