using System.Text.Json;
using System.Text.RegularExpressions;

namespace Otterkit; 

public partial struct Token
{
    public int line;
    public int column;
    public string value;
    public TokenType? type;
    public TokenScope? scope;
    public TokenContext? context;

    public Token(string value, TokenType? type, int line, int column)
    {
        this.line = line;
        this.column = column;
        this.value = value;
        this.type = type;
        scope = null;
        context = null;
    }

    public Token(string value, int line, int column)
    {
        this.line = line;
        this.column = column;
        this.value = value;
        type = null;
        scope = null;
        context = null;
    }

    private static TokenType FindType(string value)
    {
        //check if the value is a reserved keyword
        if (ParsingInfo.IsReservedWord(value.ToUpperInvariant()))
            return TokenType.ReservedKeyword;
        //check if the value is a figurative literal
        else if (ParsingInfo.IsReservedFigurativeLiteral(value.ToUpperInvariant()))
            return TokenType.FigurativeLiteral;
        //check if the value is an intrinsic function
        else if (ParsingInfo.IsIntrinsicFunctionName(value.ToUpperInvariant()))
            return TokenType.IntrinsicFunction;
        //check if the value is a symbol
        else if (ParsingInfo.IsReservedSymbol(value.ToUpperInvariant()))
            return TokenType.Symbol;
        //check if the value is a string
        else if (value.StartsWith('"'))
            return TokenType.String;
        //check if the value is a numeric
        else if (IsNumeric(value) && NumericRegex().IsMatch(value))
            return TokenType.Numeric;
        //check if the value is End Of File
        else if (value.Equals(">>IMP-EOF"))
            return TokenType.EOF;
        //if none of the above, it's an identifier
        else
            return TokenType.Identifier;
    }

    private static TokenContext? FindContext(Token token)
    {
        //check if the token belongs to a data division clause
        if (ParsingInfo.IsReservedClause(token.value))
            return TokenContext.IsClause;
        //check if the token is a statement
        else if (ParsingInfo.IsReservedStatement(token.value))
            return TokenContext.IsStatement;
        //check if the token represents a file separator   
        else if (token.line is -5)
            return TokenContext.IsEOF;
        //if none of the above, return null
        else
            return null;
    }

    private static TokenScope? FindScope(Token token, Token previousToken)
    {
        if (token.value.Equals("PROGRAM-ID"))
            return TokenScope.ProgramId;

        if (token.value.Equals("FUNCTION-ID"))
            return TokenScope.FunctionId;

        if (token.value.Equals("INTERFACE-ID"))
            return TokenScope.InterfaceId;

        if (token.value.Equals("CLASS-ID"))
            return TokenScope.ClassId;

        if (token.value.Equals("METHOD-ID"))
            return TokenScope.MethodId;

        if (token.value.Equals("ENVIRONMENT"))
            return TokenScope.EnvironmentDivision;

        if (token.value.Equals("DATA"))
            return TokenScope.DataDivision;

        if (token.value.Equals("PROCEDURE"))
            return TokenScope.ProcedureDivision;

        if (token.value.Equals("FACTORY"))
            return TokenScope.Factory;

        if (token.value.Equals("OBJECT"))
            return TokenScope.Object;

        return previousToken.scope;
    }

    public static Token FromValue(string value, int line, int column)
    {
        if (FindType(value) == TokenType.String)
            return new Token(value, TokenType.String, line, column);

        if(FindType(value) == TokenType.EOF && value.Equals(">>IMP-EOF"))
            return new Token("EOF", TokenType.EOF, -5, -5);

        return new Token(value.ToUpper(), FindType(value), line, column);
    }

    public static List<Token> FromValue(List<Token> tokens)
    {
        List<Token> newTokens = new();
        Token previousToken = new();
        foreach (Token token in tokens)
        {
            Token newToken = FromValue(token.value, token.line, token.column);
            newToken.scope = FindScope(newToken, previousToken);
            newToken.context = FindContext(newToken);
            newTokens.Add(newToken);
            previousToken = newToken;
        }

        return newTokens;
    }

    public static bool IsNumeric(ReadOnlySpan<char> value)
    {
        return value switch
        {
            ['+' or '-', '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9', .. _] => true,
            ['.', '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9', .. _] => true,
            ['0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9', .. _] => true,
            [..] => false
        };

    }

    [GeneratedRegex("^(\\+|-)?\\.?[0-9]+(\\.[0-9]+)?$", RegexOptions.ExplicitCapture | RegexOptions.NonBacktracking)]
    private static partial Regex NumericRegex();
}