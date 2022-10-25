namespace Otterkit;

public static class OtterkitClassifier
{

    public static List<Token> Classify(List<Token> tokenList)
    {

        List<string> ReservedKeywords = ClassifiedTokens.reservedKeywords;
        List<string> FigurativeLiterals = ClassifiedTokens.figurativeLiteral;
        List<string> IntrinsicFunctions = ClassifiedTokens.intrinsicFunctions;
        List<string> Symbols = ClassifiedTokens.Symbols;


        List<Token> classified = new();
        foreach (Token token in tokenList)
        {
            if (ReservedKeywords.Contains(token.token))
                classified.Add(new Token(token.token, "reserved", "", token.line, token.column));

            else if (FigurativeLiterals.Contains(token.token))
                classified.Add(new Token(token.token, "figurative literal", "", token.line, token.column));

            else if (IntrinsicFunctions.Contains(token.token))
                classified.Add(new Token(token.token, "intrinsic function", "", token.line, token.column));

            else if (Symbols.Contains(token.token))
                classified.Add(new Token(token.token, "symbol", "", token.line, token.column));

            else if (token.token.StartsWith("\""))
                classified.Add(new Token(token.token, "string", "", token.line, token.column));
            
            else if (token.token.All(Char.IsDigit))
                classified.Add(new Token(token.token, "numeric", "", token.line, token.column));

            else
                classified.Add(new Token(token.token, "identifier", "", token.line, token.column));

        }
        return classified;
    }
}