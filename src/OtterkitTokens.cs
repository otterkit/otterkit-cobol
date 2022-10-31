using System.Text.Json;
using System.Reflection;
using System.IO;

namespace Otterkit;

public enum TokenType
{
    ReservedKeyword,
    FigurativeLiteral,
    IntrinsicFunction,
    Symbol,
    String,
    Numeric,
    Identifier
}

public struct Token
{
    public string value;
    public TokenType? type;
    public string? scope;
    public int line;
    public int column;
    private static JsonElement? tokenJsonCache=null;
    public Token(
        string value,
        TokenType? type,
        string scope,
        int line,
        int column
        )
    {
        this.value = value;
        this.type = type;
        this.scope = scope;
        this.line = line;
        this.column = column;
    }
    public Token(
        string value,
        int line,
        int column
        )
    {
        this.value = value;
        this.type = null;
        this.scope = null;
        this.line = line;
        this.column = column;
    }
    public static TokenType FindType(string value,int line, int column){
        //get Otterkit.paringinfo.json from assembly
        if(tokenJsonCache==null){
            Assembly assembly = (Assembly)Assembly.GetEntryAssembly();
            Stream stream = (Stream)assembly.GetManifestResourceStream("Otterkit.parsinginfo.json");
            StreamReader reader = new System.IO.StreamReader(stream);
            tokenJsonCache = JsonSerializer.Deserialize<JsonElement>(reader.ReadToEnd());
        }
        
        JsonElement tokenJson = (JsonElement)tokenJsonCache;
        //check if the value is a reserved keyword
        if(tokenJson.GetProperty("reservedKeywords").EnumerateArray().Any(x=>x.GetString()==value))
            return TokenType.ReservedKeyword;
        //check if the value is a figurative literal
        else if(tokenJson.GetProperty("figurativeLiteral").EnumerateArray().Any(x=>x.GetString()==value))
            return TokenType.FigurativeLiteral;
        //check if the value is an intrinsic function
        else if(tokenJson.GetProperty("intrinsicFunctions").EnumerateArray().Any(x=>x.GetString()==value))
            return TokenType.IntrinsicFunction;
        //check if the value is a symbol
        else if(tokenJson.GetProperty("Symbols").EnumerateArray().Any(x=>x.GetString()==value))
            return TokenType.Symbol;
        //check if the value is a string
        else if(value.StartsWith("\""))
            return TokenType.String;
        //check if the value is a numeric
        else if(value.All(Char.IsDigit))
            return TokenType.Numeric;
        //if none of the above, it's an identifier
        else
            return TokenType.Identifier;

    }
    public static Token fromValue(string value,int line, int column){
        return new Token(value,Token.FindType(value,line,column),"",line,column);
    }
    public static List<Token> fromValue(List<Token> tokens){
        List<Token> newTokens = new List<Token>();
        foreach(Token token in tokens){
            newTokens.Add(Token.fromValue(token.value,token.line,token.column));
        }
        return newTokens;
    }
    public static IEnumerable<Token> fromValue(IEnumerable<Token> tokens){
        return Token.fromValue(tokens.ToList());
    }
    public static Token[] fromValue(Token[] tokens){
        return Token.fromValue(tokens.ToList()).ToArray();
    }
    
}