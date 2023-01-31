using System.Text.RegularExpressions;

namespace Otterkit; 

public partial record Token
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
        if (ParsingInfo.IsReservedWord(value))
            return TokenType.ReservedKeyword;
        //check if the value is a figurative literal
        else if (ParsingInfo.IsReservedFigurativeLiteral(value))
            return TokenType.FigurativeLiteral;
        //check if the value is an intrinsic function
        else if (ParsingInfo.IsIntrinsicFunctionName(value))
            return TokenType.IntrinsicFunction;
        //check if the value is a symbol
        else if (ParsingInfo.IsReservedSymbol(value))
            return TokenType.Symbol;
        //check if the value is a string
        else if (value.StartsWith('"') || value.StartsWith('\''))
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
        if (token.value.Equals("PROGRAM-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.ProgramId;

        if (token.value.Equals("FUNCTION-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.FunctionId;

        if (token.value.Equals("INTERFACE-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.InterfaceId;

        if (token.value.Equals("CLASS-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.ClassId;

        if (token.value.Equals("METHOD-ID", StringComparison.OrdinalIgnoreCase))
            return TokenScope.MethodId;

        if (token.value.Equals("ENVIRONMENT", StringComparison.OrdinalIgnoreCase))
            return TokenScope.EnvironmentDivision;

        if (token.value.Equals("DATA", StringComparison.OrdinalIgnoreCase))
            return TokenScope.DataDivision;

        if (token.value.Equals("PROCEDURE", StringComparison.OrdinalIgnoreCase))
            return TokenScope.ProcedureDivision;

        if (token.value.Equals("FACTORY", StringComparison.OrdinalIgnoreCase))
            return TokenScope.Factory;

        if (token.value.Equals("OBJECT", StringComparison.OrdinalIgnoreCase))
            return TokenScope.Object;

        return previousToken.scope;
    }

    public static List<Token> FromValue(List<Token> tokens)
    {
        Token previousToken = tokens[0];
        foreach (Token token in tokens)
        {
            token.type = FindType(token.value);
            if(token.type == TokenType.EOF && token.value.Equals(">>IMP-EOF"))
            {
                token.value = "EOF";
                token.line = -5;
                token.column = -5;
            }
            token.scope = FindScope(token, previousToken);
            token.context = FindContext(token);
            previousToken = token;
        }

        return tokens;
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