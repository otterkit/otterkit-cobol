namespace OtterkitLibrary;

public class Signature
{
    internal ProcedurePointer Pointer { get; init; }
    internal long Hash { get; init; }

    public Signature(ProcedurePointer pointer, long hash)
    {
        this.Pointer = pointer;
        this.Hash = hash;
    }
}
