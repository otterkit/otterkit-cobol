namespace Otterkit; 

public sealed partial record Token
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
}