using System.Text.RegularExpressions;

namespace Otterkit;
public static class OtterkitLexer
{
    public static void Tokenize(List<string> sourceLines)
    {
        int lineNumber = 0;
        string pattern = 
            "\\w+(-[\\w0-9]+)*|(\")(.*?)(\"|$)|(\')(.*?)(\'|$)|\\w+|[^\\w\\s]|(\r\n|\r|\n)";
        /*  
        **  Explaining the big regex pattern:
        **
        **  < \w+(-[\w0-9]+)* > : Matches all COBOL reserved keywords and
        **                        intrinsic function names
        **
        **  < (\")(.*?)(\"|$) > :
        **  < (\')(.*?)(\'|$) > :
        **          Matches all strings including fixed format continuation lines,
        **          where the string ends without closing the quote and continues
        **          with a -" on the next line. Useful for preserving string formatting
        **   
        **  < \w+ > : Matches any single words. Potentially useful for identifiers
        **  
        **  < [^\w\s] > : Matches all single char special symbols, except the _ character 
        **  
        **  < (\r\n|\r|\n)" > : Matches CRLF, CR and LF. New line characters
        */

        foreach (string line in sourceLines)
        {
            lineNumber += 1;
            foreach (Match token in Regex.Matches(line, pattern, RegexOptions.IgnoreCase))
            {
                Console.WriteLine("<Line {0}: Column {1}>: {2}", lineNumber, token.Index, token.Value);
            }

        }
    }
}