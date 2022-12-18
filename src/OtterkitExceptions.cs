using System.Runtime.Serialization;

namespace OtterkitLibrary;

[Serializable]
public sealed class EcArgumentFunction : Exception
{
    static readonly string defaultError = "EC-ARGUMENT-FUNCTION: Function argument error";
    public EcArgumentFunction()
        : base(defaultError) { }

    public EcArgumentFunction(string message)
        : base(message) { }

    public EcArgumentFunction(string message, Exception inner)
        : base(message, inner) { }

    private EcArgumentFunction(SerializationInfo info, StreamingContext context)
        : base(info, context) { }
}

[Serializable]
public sealed class EcDataPtrNull : Exception
{
    static readonly string defaultError = "EC-DATA-PTR-NULL: Based item data-pointer was null when referenced";
    public EcDataPtrNull()
        : base(defaultError) { }

    public EcDataPtrNull(string message)
        : base(message) { }

    public EcDataPtrNull(string message, Exception inner)
        : base(message, inner) { }

    private EcDataPtrNull(SerializationInfo info, StreamingContext context)
        : base(info, context) { }
}

[Serializable]
public sealed class EcBoundPtr : Exception
{
    static readonly string defaultError = "EC-BOUND-PTR: Data-pointer contains an address that is out of bounds";
    public EcBoundPtr()
        : base(defaultError) { }

    public EcBoundPtr(string message)
        : base(message) { }

    public EcBoundPtr(string message, Exception inner)
        : base(message, inner) { }

    private EcBoundPtr(SerializationInfo info, StreamingContext context)
        : base(info, context) { }
}

[Serializable]
public sealed class EcExternalFormatConflict : Exception
{
    static readonly string defaultError = """
    EC-EXTERNAL-FORMAT-CONFLICT: Current external definitions are no compatible with each other. External definitions that share the same name must also have the same byte length and same default value
    """;
    public EcExternalFormatConflict()
        : base(defaultError) { }

    public EcExternalFormatConflict(string message)
        : base(message) { }

    public EcExternalFormatConflict(string message, Exception inner)
        : base(message, inner) { }

    private EcExternalFormatConflict(SerializationInfo info, StreamingContext context)
        : base(info, context) { }
}
