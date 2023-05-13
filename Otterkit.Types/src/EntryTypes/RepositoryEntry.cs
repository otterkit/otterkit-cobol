using Otterkit.Types;

namespace Otterkit;

public class RepositoryName
{
    public Option<Token> Identifier;
    public Option<string> ExternalizedName;

    public SourceUnit SourceType;

    public int DeclarationIndex;

    public RepositoryName(Token identifier) 
    {
        Identifier = identifier;
    }
}
