using System.Text.RegularExpressions;

namespace Otterkit;
public static class OtterkitClassifier
{
    public static List<Token> Classify(List<Token> tokenList)
    {
        List<Token> classified = new();
        foreach (Token token in tokenList)
        {
            if (ClassifiedTokens.reservedKeywords.Contains(token.token))
                classified.Add(new Token(token.token, "reserved", "", token.line, token.column));

            if (ClassifiedTokens.figurativeLiteral.Contains(token.token))
                classified.Add(new Token(token.token, "figurative literal", "", token.line, token.column));

            if (ClassifiedTokens.intrinsicFunctions.Contains(token.token))
                classified.Add(new Token(token.token, "intrinsic function", "", token.line, token.column));

            if (token.token.StartsWith("\""))
                classified.Add(new Token(token.token, "string", "", token.line, token.column));
            
            else if (Regex.IsMatch(token.token, @"^\d"))
                classified.Add(new Token(token.token, "numeric", "", token.line, token.column));

            else
            {
                // Still missing especial symbols, currently they classify as "indentifier"
                // TODO: Refactor this if..else and add symbol classification.
                classified.Add(new Token(token.token, "identifier", "", token.line, token.column));
            }

        }
        return classified;
    }
}