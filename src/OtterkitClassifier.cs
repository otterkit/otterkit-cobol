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
            if (ReservedKeywords.Contains(token.value.ToUpper()))
                classified.Add(new Token(token.value.ToUpper(), "reserved", "", token.line, token.column));

            else if (FigurativeLiterals.Contains(token.value.ToUpper()))
                classified.Add(new Token(token.value.ToUpper(), "figurative literal", "", token.line, token.column));

            else if (IntrinsicFunctions.Contains(token.value.ToUpper()))
                classified.Add(new Token(token.value.ToUpper(), "intrinsic function", "", token.line, token.column));

            else if (Symbols.Contains(token.value))
                classified.Add(new Token(token.value, "symbol", "", token.line, token.column));

            else if (token.value.StartsWith("\""))
                classified.Add(new Token(token.value, "string literal", "", token.line, token.column));
            
            else if (token.value.All(Char.IsDigit))
                classified.Add(new Token(token.value, "number literal", "", token.line, token.column));

            else
                classified.Add(new Token(token.value, "identifier", "", token.line, token.column));

        }
        classified.Add(new Token("EOF", "EOF", "EOF", -1, -1));
        return classified;
    }
}