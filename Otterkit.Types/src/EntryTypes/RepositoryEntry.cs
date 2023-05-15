using Otterkit.Types;

namespace Otterkit;

public class RepositoryName
{
    public Token Identifier;
    public UnitKind SourceType;
    public int DeclarationIndex;

    public RepositoryName(Token identifier) 
    {
        Identifier = identifier;
    }
}
