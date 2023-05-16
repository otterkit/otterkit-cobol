namespace Otterkit.Types;

public abstract class AbstractEntry
{
    public Option<Token> Identifier;
    public EntryKind EntryKind;

    protected ulong BitField;
    public int DeclarationIndex;

    protected AbstractEntry(Token identifier, EntryKind entryKind)
    {
        Identifier = identifier;
        EntryKind = entryKind;
    }

    protected void SetBit(int position, bool bit)
    {
        var mask = 1UL << position - 1;

        if (bit)
        {
            BitField |= mask;
            return;
        }

        BitField &= ~mask;
    }

    protected bool GetBit(int position)
    {
        var bit = (BitField >> (position - 1)) & 1;

        return bit == 1UL;
    }
}
