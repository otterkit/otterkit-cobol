using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace OtterkitLibrary;

public static partial class External
{
    private static readonly Dictionary<string, Signature> ExternalRepository = new();

    public static void RegisterSignature(string name, ProcedurePointer pointer, long hash)
    {
        ExternalRepository[name] = new Signature(pointer, hash);
    }

    public static Signature? GetSignature(string name)
    {
        ref var signature = ref CollectionsMarshal.GetValueRefOrNullRef(ExternalRepository, name);

        if (Unsafe.IsNullRef(ref signature))
        {
            ExceptionRegistry.ActivateException("EC-PROGRAM-NOT-FOUND");
            return null;
        }

        return signature;
    }

    public static void Invoke(ProcedurePointer pointer, out ICOBOLType? returning, params ICOBOLType[] args)
    {
        var invoke = pointer(out var ret, args);

        if (invoke is false) ExceptionRegistry.ActivateException("EC-PROGRAM-ARG-MISMATCH");

        returning = ret;
    }
}
