using System.Text.RegularExpressions;

namespace Otterkit;
public static class OtterkitLexer
{
    public static void Tokenize(List<string> sourceLines)
    {
        int lineNumber = 0;
        string pattern = "\\w+(-[\\w0-9]+)*|(^-\\s* \"|\")(.*?)(\"|(\r\n|\r|\n)|$)|\\w+|[^\\w\\s]|(\r\n|\r|\n)";
        foreach (string line in sourceLines)
        {
            lineNumber += 1;
            foreach (Match match in Regex.Matches(line, pattern, RegexOptions.IgnoreCase))
            {
                Console.WriteLine("<Line {0}: Column {1}>: {2}", lineNumber, match.Index, match.Value);
            }

        }
    }
}