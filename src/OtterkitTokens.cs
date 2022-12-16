using System.Text.Json;
using System.Reflection;
using System.Text.RegularExpressions;

namespace Otterkit;

public enum TokenType
{
    ReservedKeyword,
    FigurativeLiteral,
    IntrinsicFunction,
    Symbol,
    String,
    Numeric,
    Identifier,
    Expression,
    Device,
    EOF
}

public enum TokenScope
{
    ProgramId,
    FunctionId,
    InterfaceId,
    ClassId,
    MethodId,
    Factory,
    Object,
    EnvironmentDivision,
    DataDivision,
    ProcedureDivision
}

public enum TokenContext
{
    IsClause,
    IsStatement
}

public struct Token
{
    public int line;
    public int column;
    public string value;
    public TokenType? type;
    public TokenScope? scope;
    public TokenContext? context;
    private static JsonElement? tokenJsonCache = null;
    public Token(string value, TokenType? type, int line, int column)
    {
        this.line = line;
        this.column = column;
        this.value = value;
        this.type = type;
        this.scope = null;
        this.context = null;
    }

    public Token(string value, int line, int column)
    {
        this.line = line;
        this.column = column;
        this.value = value;
        this.type = null;
        this.scope = null;
        this.context = null;
    }

    public static TokenType FindType(string value)
    {
        //get Otterkit.paringinfo.json from assembly
        if (tokenJsonCache == null)
        {
            Assembly assembly = Assembly.GetCallingAssembly();
            Stream? stream = assembly.GetManifestResourceStream("Otterkit.parsinginfo.json");
            StreamReader reader = new System.IO.StreamReader(stream == null ? throw new ArgumentNullException() : stream);
            tokenJsonCache = JsonSerializer.Deserialize<JsonElement>(reader.ReadToEnd());
        }

        JsonElement tokenJson = (JsonElement)tokenJsonCache;
        //check if the value is a reserved keyword
        if (tokenJson.GetProperty("reservedKeywords").EnumerateArray().Any(json => json.GetString() == value.ToUpper()))
            return TokenType.ReservedKeyword;
        //check if the value is a figurative literal
        else if (tokenJson.GetProperty("figurativeLiteral").EnumerateArray().Any(json => json.GetString() == value.ToUpper()))
            return TokenType.FigurativeLiteral;
        //check if the value is an intrinsic function
        else if (tokenJson.GetProperty("intrinsicFunctions").EnumerateArray().Any(json => json.GetString() == value.ToUpper()))
            return TokenType.IntrinsicFunction;
        //check if the value is a symbol
        else if (tokenJson.GetProperty("symbols").EnumerateArray().Any(json => json.GetString() == value))
            return TokenType.Symbol;
        //check if the value is a string
        else if (value.StartsWith("\""))
            return TokenType.String;
        //check if the value is a numeric
        else if (Regex.IsMatch(value, @"^(\+|-)?\.?[0-9]\d*(\.\d+)?"))
            return TokenType.Numeric;
        //check if the value is End Of File
        else if (value == "EOF")
            return TokenType.EOF;
        //if none of the above, it's an identifier
        else
            return TokenType.Identifier;
    }

    public static TokenContext? FindContext(Token token)
    {
        //get Otterkit.paringinfo.json from assembly
        if (tokenJsonCache == null)
        {
            Assembly assembly = Assembly.GetCallingAssembly();
            Stream? stream = assembly.GetManifestResourceStream("Otterkit.parsinginfo.json");
            StreamReader reader = new System.IO.StreamReader(stream == null ? throw new ArgumentNullException() : stream);
            tokenJsonCache = JsonSerializer.Deserialize<JsonElement>(reader.ReadToEnd());
        }

        JsonElement tokenJson = (JsonElement)tokenJsonCache;
        //check if the token belongs to a data division clause
        if (tokenJson.GetProperty("clauses").EnumerateArray().Any(json => json.GetString() == token.value))
            return TokenContext.IsClause;
        //check if the token is a statement
        else if (tokenJson.GetProperty("statements").EnumerateArray().Any(json => json.GetString() == token.value))
            return TokenContext.IsStatement;
        //if none of the above, return null
        else
            return null;
    }

    public static TokenScope? FindScope(Token token, Token previousToken)
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

        if (token.value.Equals("FACTORY"))
            return TokenScope.Factory;

        if (token.value.Equals("OBJECT"))
            return TokenScope.Object;

        if (token.value.Equals("ENVIRONMENT"))
            return TokenScope.EnvironmentDivision;

        if (token.value.Equals("DATA"))
            return TokenScope.DataDivision;

        if (token.value.Equals("PROCEDURE"))
            return TokenScope.ProcedureDivision;

        return previousToken.scope;
    }

    public static Token fromValue(string value, int line, int column)
    {
        if (Token.FindType(value) == TokenType.String)
            return new Token(value, Token.FindType(value), line, column);

        return new Token(value.ToUpper(), Token.FindType(value), line, column);
    }

    public static List<Token> fromValue(List<Token> tokens)
    {
        List<Token> newTokens = new List<Token>();
        Token previousToken = new();
        foreach (Token token in tokens)
        {
            Token newToken = Token.fromValue(token.value, token.line, token.column);
            newToken.scope = FindScope(newToken, previousToken);
            newToken.context = FindContext(newToken);
            newTokens.Add(newToken);
            previousToken = newToken;
        }
        newTokens.Add(Token.fromValue("EOF", -1, -1));
        return newTokens;
    }

    public static IEnumerable<Token> fromValue(IEnumerable<Token> tokens)
    {
        return Token.fromValue(tokens.ToList());
    }

    public static Token[] fromValue(Token[] tokens)
    {
        return Token.fromValue(tokens.ToList()).ToArray();
    }

}