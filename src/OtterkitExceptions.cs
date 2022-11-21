using System.Runtime.Serialization;

namespace OtterkitLibrary;

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
