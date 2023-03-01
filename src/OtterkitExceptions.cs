using System.Runtime.Serialization;

namespace OtterkitLibrary;

//Exceptions as objects similar to C# exceptions are currently depreciated. May return to implement COBOL exception objects.


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


// Implementing COBOL default/regular exceptions as a singleton containing a dictionary of exception status indicators

public static class ExceptionRegistry
{
    public enum IsFatal
    { 
        //why an enum instead of a bool? So that other programmers can understand what this actually means
        Fatal,
        NonFatal,
        Other //exceptions with labels greater than three do not seem to be set as either fata or nonfatal
    }

    private sealed class ExceptionMetadata
    {
        public bool IsActivated { get; set; }

        public bool IsChecked { get; set; } //yes, you can turn checking for default exceptions off.

        public IsFatal FatalityState { get; private set; }

        public ExceptionMetadata(bool isActivated, bool isChecked, IsFatal fatalityType)
        {
            this.IsActivated = isActivated;
            this.IsChecked = isChecked;
            this.FatalityState = fatalityType;
        }
    }

    private static ExceptionMetadata ChangeStatus(string name, bool condition)
    {
        ExceptionMetadata current = ExceptionRegistry.registry[name];
        current.IsActivated = condition;
        return current;
    }

    private static void ChangeChecked(string name, bool checkedCondition)
    {
        ExceptionRegistry.registry[name].IsChecked = checkedCondition;
    }

    private static bool IsExceptionChecked(string name)
    {
        return ExceptionRegistry.registry[name].IsChecked;
    }

    public static void CheckOn(string name)
    {
        ChangeChecked(name, true);
    }

    public static void CheckOff(string name)
    {
        ChangeChecked(name, false);
    }

    public static void ActivateException(string name)
    {
        if (IsExceptionChecked(name))
        {
            LastException = ChangeStatus(name, true);
        }
    }

    public static void DeactivateException(string name)
    {
        ChangeStatus(name, false);
    }
    /*
    As of writing this comment, activateException() and deactivateException() are sort of black boxes from an outside view: they perform what you think they would do, but do not
    care about if the exception was already on or off. Addtionally, activateException() only activates if the exception is being actively checked. This probably fits the
    behavior of the standard, but is this a good abstraction?
    */

    private static ExceptionMetadata? LastException { get; set; }

    private static Dictionary<string, ExceptionMetadata> registry = new(StringComparer.OrdinalIgnoreCase)
    {
        /*
        It seems that all regular exceptions are uppercase
        All exceptions are false and checked for by default
        I thought about metaprogramming this block, but any solution would just be as time-consuming.
        All exceptions where it's up to the implementor to make the call(imp) are fatal by default for now.
        See the standard for more information.
        */
        {"EC-ALL",                      new (false, true, IsFatal.Other)},
        {"EC-ARGUMENT",                 new (false, true, IsFatal.Other)},
        {"EC-ARGUMENT-FUNCTION",        new (false, true, IsFatal.Fatal)},
        {"EC-ARGUMENT-IMP",             new (false, true, IsFatal.Fatal)},//imp
        {"EC-BOUND",                    new (false, true, IsFatal.Other)},
        {"EC-BOUND-FUNC-RET-VALUE",     new (false, true, IsFatal.NonFatal)},
        {"EC-BOUND-IMP",                new (false, true, IsFatal.Fatal)},//imp
        {"EC-BOUND-ODO",                new (false, true, IsFatal.Fatal)},
        {"EC-BOUND-OVERFLOW",           new (false, true, IsFatal.NonFatal)},
        {"EC-BOUND-PTR",                new (false, true, IsFatal.Fatal)},
        {"EC-BOUND-REF-MOD",            new (false, true, IsFatal.Fatal)},
        {"EC-BOUND-SET",                new (false, true, IsFatal.NonFatal)},
        {"EC-BOUND-SUBSCRIPT",          new (false, true, IsFatal.Fatal)},
        {"EC-BOUND-TABLE-LIMIT",        new (false, true, IsFatal.Fatal)},
        {"EC-CONTINUE",                 new (false, true, IsFatal.Other)},
        {"EC-CONTINUE-IMP",             new (false, true, IsFatal.Fatal)}, //imp 
        {"EC-CONTINUE-LESS-THAN-ZERO",  new (false, true, IsFatal.NonFatal)},
        {"EC-DATA",                     new (false, true, IsFatal.Other)},
        {"EC-DATA-CONVERSION",          new (false, true, IsFatal.NonFatal)},
        {"EC-DATA-IMP",                 new (false, true, IsFatal.Fatal)}, //imp
        {"EC-DATA-INCOMPATIBLE",        new (false, true, IsFatal.Fatal)},
        {"EC-DATA-NOT-FINITE",          new (false, true, IsFatal.Fatal)},
        {"EC-DATA-OVERFLOW",            new (false, true, IsFatal.Fatal)},
        {"EC-DATA-PTR-NULL",            new (false, true, IsFatal.Fatal)},
        {"EC-EXTERNAL",                 new (false, true, IsFatal.Other)},
        {"EC-EXTERNAL-DATA-MISMATCH",   new (false, true, IsFatal.Fatal)},
        {"EC-EXTERNAL-FILE-MISMATCH",   new (false, true, IsFatal.Fatal)},
        {"EC-EXTERNAL-FORMAT-CONFLICT", new (false, true, IsFatal.Fatal)},
        {"EC-EXTERNAL-IMP",             new (false, true, IsFatal.NonFatal)}, //imp
        {"EC-FLOW",                     new (false, true, IsFatal.Other)},
        {"EC-FLOW-APPLY-COMMIT",        new (false, true, IsFatal.Fatal)},
        {"EC-FLOW-COMMIT",              new (false, true, IsFatal.Fatal)},
        {"EC-FLOW-GLOBAL-EXIT",         new (false, true, IsFatal.Fatal)},
        {"EC-FLOW-GLOBAL-GOBACK",       new (false, true, IsFatal.Fatal)},
        {"EC-FLOW-IMP",                 new (false, true, IsFatal.Fatal)}, //imp
        {"EC-FLOW-RELEASE",             new (false, true, IsFatal.Fatal)},
        {"EC-FLOW-REPORT",              new (false, true, IsFatal.Fatal)},
        {"EC-FLOW-RETURN",              new (false, true, IsFatal.Fatal)},
        {"EC-FLOW-ROLLBACK",            new (false, true, IsFatal.Fatal)},
        {"EC-FLOW-SEARCH",              new (false, true, IsFatal.Fatal)},
        {"EC-FLOW-USE",                 new (false, true, IsFatal.Fatal)},
        {"EC-FUNCTION",                 new (false, true, IsFatal.Other)},
        {"EC-FUNCTION-ARG-OMITTED",     new (false, true, IsFatal.Other)},
        {"EC-FUNCTION-IMP",             new (false, true, IsFatal.Fatal)}, //imp
        {"EC-FUNCTION-NOT-FOUND",       new (false, true, IsFatal.Fatal)},
        {"EC-FUNCTION-PTR-INVALID",     new (false, true, IsFatal.Fatal)},
        {"EC-FUNCTION-PTR-NULL",        new (false, true, IsFatal.Fatal)},
        {"EC-I-O",                      new (false, true, IsFatal.Other)},
        {"EC-I-O-AT-END",               new (false, true, IsFatal.NonFatal)},
        {"EC-I-O-EOP",                  new (false, true, IsFatal.NonFatal)},
        {"EC-I-O-EOP-OVERFLOW",         new (false, true, IsFatal.NonFatal)},
        {"EC-I-O-FILE-SHARING",         new (false, true, IsFatal.NonFatal)},
        {"EC-I-O-IMP",                  new (false, true, IsFatal.Fatal)}, //imp
        {"EC-I-O-INVALID-KEY",          new (false, true, IsFatal.NonFatal)},
        {"EC-I-O-LINAGE",               new (false, true, IsFatal.Fatal)},
        {"EC-I-O-LOGIC-ERROR",          new (false, true, IsFatal.Fatal)},
        {"EC-I-O-PERMANENT-ERROR",      new (false, true, IsFatal.Fatal)},
        {"EC-I-O-RECORD-CONTENT",       new (false, true, IsFatal.NonFatal)},
        {"EC-I-O-RECORD-OPERATION",     new (false, true, IsFatal.NonFatal)},
        {"EC-I-O-WARNING",              new (false, true, IsFatal.NonFatal)},
        {"EC-IMP",                      new (false, true, IsFatal.Other)},

        /*
        This entry is unique, so this comment is to bring attention to this situation. Accoridng to the standard, 
        the "suffix" part is defined by the implementor as a custom level-3 exception with the level-2 naturally being 
        "EC-IMP". So there needs to be work done to decide what this exception should be.
        */
        {"EC-IMP-suffix",               new (false, true, IsFatal.Other)},
        {"EC-LOCALE",                   new (false, true, IsFatal.Other)},
        {"EC-LOCALE-IMP",               new (false, true, IsFatal.Fatal)}, //imp
        {"EC-LOCALE-INCOMPATIBLE",      new (false, true, IsFatal.Fatal)},
        {"EC-LOCALE-INVALID",           new (false, true, IsFatal.Fatal)},
        {"EC-LOCALE-INVALID-PTR",       new (false, true, IsFatal.Fatal)},
        {"EC-LOCALE-MISSING",           new (false, true, IsFatal.Fatal)},
        {"EC-LOCALE-SIZE",              new (false, true, IsFatal.Fatal)},
        {"EC-MCS",                      new (false, true, IsFatal.Other)},
        {"EC-MCS-ABNORMAL-TERMINATION", new (false, true, IsFatal.NonFatal)},
        {"EC-MCS-IMP",                  new (false, true, IsFatal.Fatal)}, //imp
        {"EC-MCS-INVALID-TAG",          new (false, true, IsFatal.NonFatal)},
        {"EC-MCS-MESSAGE-LENGTH",       new (false, true, IsFatal.NonFatal)},
        {"EC-MCS-NO-REQUESTER",         new (false, true, IsFatal.NonFatal)},
        {"EC-MCS-NO-SERVER",            new (false, true, IsFatal.NonFatal)},
        {"EC-MCS-NORMAL-TERMINATION",   new (false, true, IsFatal.NonFatal)},
        {"EC-MCS-REQUESTOR-FAILED",     new (false, true, IsFatal.NonFatal)},
        {"EC-OO",                       new (false, true, IsFatal.Other)},
        {"EC-OO-ARG-OMITTED",           new (false, true, IsFatal.Fatal)},
        {"EC-OO-CONFORMANCE",           new (false, true, IsFatal.Fatal)},
        {"EC-OO-EXCEPTION",             new (false, true, IsFatal.Fatal)},
        {"EC-OO-IMP",                   new (false, true, IsFatal.Fatal)}, //imp
        {"EC-OO-METHOD",                new (false, true, IsFatal.Fatal)},
        {"EC-OO-NULL",                  new (false, true, IsFatal.Fatal)},
        {"EC-OO-RESOURCE",              new (false, true, IsFatal.Fatal)},
        {"EC-OO-UNIVERSAL",             new (false, true, IsFatal.Fatal)},
        {"EC-ORDER",                    new (false, true, IsFatal.Other)},
        {"EC-ORDER-IMP",                new (false, true, IsFatal.Fatal)}, //imp
        {"EC-ORDER-NOT-SUPPORTED",      new (false, true, IsFatal.Fatal)},
        {"EC-OVERFLOW",                 new (false, true, IsFatal.Other)},
        {"EC-OVERFLOW-IMP",             new (false, true, IsFatal.Fatal)}, //imp
        {"EC-OVERFLOW-STRING",          new (false, true, IsFatal.NonFatal)},
        {"EC-OVERFLOW-UNSTRING",        new (false, true, IsFatal.NonFatal)},
        {"EC-PROGRAM",                  new (false, true, IsFatal.Other)},
        {"EC-PROGRAM-ARG-MISMATCH",     new (false, true, IsFatal.Fatal)},
        {"EC-PROGRAM-ARG-OMITTED",      new (false, true, IsFatal.Fatal)},
        {"EC-PROGRAM-CANCEL-ACTIVE",    new (false, true, IsFatal.Fatal)},
        {"EC-PROGRAM-IMP",              new (false, true, IsFatal.Fatal)}, //imp
        {"EC-PROGRAM-NOT-FOUND",        new (false, true, IsFatal.Fatal)}, //imp
        {"EC-PROGRAM-PTR-NULL",         new (false, true, IsFatal.Fatal)},
        {"EC-PROGRAM-RECURSIVE-CALL",   new (false, true, IsFatal.Fatal)},
        {"EC-PROGRAM-RESOURCES",        new (false, true, IsFatal.Fatal)},
        {"EC-RAISING",                  new (false, true, IsFatal.Other)},
        {"EC-RAISING-IMP",              new (false, true, IsFatal.Fatal)}, //imp
        {"EC-RAISING-NOT-SPECIFIED",    new (false, true, IsFatal.Fatal)},
        {"EC-RANGE",                    new (false, true, IsFatal.Other)},
        {"EC-RANGE-IMP",                new (false, true, IsFatal.Fatal)}, //imp
        {"EC-RANGE-INDEX",              new (false, true, IsFatal.Fatal)},
        {"EC-RANGE-INSPECT-SIZE",       new (false, true, IsFatal.Fatal)},
        {"EC-RANGE-INVALID",            new (false, true, IsFatal.NonFatal)},
        {"EC-RANGE-PERFORM-VARYING",    new (false, true, IsFatal.Fatal)},
        {"EC-RANGE-PTR",                new (false, true, IsFatal.Fatal)},
        {"EC-RANGE-SEARCH-INDEX",       new (false, true, IsFatal.NonFatal)},
        {"EC-RANGE-SEARCH-NO-MATCH",    new (false, true, IsFatal.NonFatal)},
        {"EC-REPORT",                   new (false, true, IsFatal.Other)},
        {"EC-REPORT-ACTIVE",            new (false, true, IsFatal.Fatal)},
        {"EC-REPORT-COLUMN-OVERLAP",    new (false, true, IsFatal.NonFatal)},
        {"EC-REPORT-FILE-MODE",         new (false, true, IsFatal.Fatal)},
        {"EC-REPORT-IMP",               new (false, true, IsFatal.Fatal)}, //imp
        {"EC-REPORT-INACTIVE",          new (false, true, IsFatal.Fatal)},
        {"EC-REPORT-LINE-OVERLAP",      new (false, true, IsFatal.NonFatal)},
        {"EC-REPORT-NOT-TERMINATED",    new (false, true, IsFatal.NonFatal)},
        {"EC-REPORT-PAGE-LIMIT",        new (false, true, IsFatal.NonFatal)},
        {"EC-REPORT-PAGE-WIDTH",        new (false, true, IsFatal.NonFatal)},
        {"EC-REPORT-SUM-SIZE",          new (false, true, IsFatal.Fatal)},
        {"EC-REPORT-VARYING",           new (false, true, IsFatal.Fatal)},
        {"EC-SCREEN",                   new (false, true, IsFatal.Other)},
        {"EC-SCREEN-FIELD-OVERLAP",     new (false, true, IsFatal.NonFatal)},
        {"EC-SCREEN-IMP",               new (false, true, IsFatal.Fatal)}, //imp
        {"EC-SCREEN-ITEM-TRUNCATED",    new (false, true, IsFatal.NonFatal)},
        {"EC-SCREEN-LINE-NUMBER",       new (false, true, IsFatal.NonFatal)},
        {"EC-SCREEN-STARTING-COLUMN",   new (false, true, IsFatal.NonFatal)},
        {"EC-SIZE",                     new (false, true, IsFatal.Other)},
        {"EC-SIZE-ADDRESS",             new (false, true, IsFatal.Fatal)},
        {"EC-SIZE-EXPONENTIATION",      new (false, true, IsFatal.Fatal)},
        {"EC-SIZE-IMP",                 new (false, true, IsFatal.Fatal)}, //imp
        {"EC-SIZE-OVERFLOW",            new (false, true, IsFatal.Fatal)},
        {"EC-SIZE-TRUNCATION",          new (false, true, IsFatal.Fatal)},
        {"EC-SIZE-UNDERFLOW",           new (false, true, IsFatal.Fatal)},
        {"EC-SIZE-ZERO-DIVIDE",         new (false, true, IsFatal.Fatal)},
        {"EC-SORT-MERGE",               new (false, true, IsFatal.Other)},
        {"EC-SORT-MERGE-ACTIVE",        new (false, true, IsFatal.Fatal)},
        {"EC-SORT-MERGE-FILE-OPEN",     new (false, true, IsFatal.Fatal)},
        {"EC-SORT-MERGE-IMP",           new (false, true, IsFatal.Fatal)}, //imp
        {"EC-SORT-MERGE-RELEASE",       new (false, true, IsFatal.Fatal)},
        {"EC-SORT-MERGE-RETURN",        new (false, true, IsFatal.Fatal)},
        {"EC-SORT-MERGE-SEQUENCE",      new (false, true, IsFatal.Fatal)},
        {"EC-STORAGE",                  new (false, true, IsFatal.Other)},
        {"EC-STORAGE-IMP",              new (false, true, IsFatal.Fatal)}, //imp
        {"EC-STORAGE-NOT-ALLOC",        new (false, true, IsFatal.NonFatal)},
        {"EC-STORAGE-NOT-AVAIL",        new (false, true, IsFatal.NonFatal)},
        {"EC-USER",                     new (false, true, IsFatal.Other)}, //user-defined

        //similar to the previous suffix exception, but this time the user decides
        {"EC-USER-suffix",              new (false, true, IsFatal.NonFatal)}, 
        {"EC-VALIDATE",                 new (false, true, IsFatal.Other)},
        {"EC-VALIDATE-CONTENT",         new (false, true, IsFatal.NonFatal)},
        {"EC-VALIDATE-FORMAT",          new (false, true, IsFatal.NonFatal)},
        {"EC-VALIDATE-IMP",             new (false, true, IsFatal.Fatal)}, //imp
        {"EC-VALIDATE-RELATION",        new (false, true, IsFatal.NonFatal)},
        {"EC-VALIDATE-VARYING",         new (false, true, IsFatal.Fatal)}
    };
}