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

public static class exceptionRegistry {
    public enum isFatal{ //why an enum instead of a bool? So that other programmers can understand what this actually means
            Fatal,
            nonFatal,

            other //exceptions with lebels greater than three do not seem to be set as either fata or nonfatal
        }

    private sealed class exception{

        public string name{get; set;} //technically not needed, but just in case a refactor changes things
        public bool isActivated {get; set;}

        public bool isChecked {get; set;} //yes, you can turn checking for default exceptions off. If anyone has a problem with this COBOL feature, take it to the standard committee
        

        public isFatal fatalityState {get; private set;}

        public exception(string name, bool isActivated, bool isChecked, string fatalityType){
            this.name = name;
            this.isActivated = isActivated;
            this.isChecked = isChecked;
            string fatalityState = fatalityType.ToLower();

            switch(fatalityState){
                case "fatal":
                    this.fatalityState = isFatal.Fatal;
                    break;
                case "nonfatal":
                    this.fatalityState = isFatal.nonFatal;
                    break;
                case "other":
                    this.fatalityState = isFatal.other; //other is for higher-level exceptions, which take on the status of the actual lower-level exception called
                    break;
                default:
                    throw new ArgumentException(String.Format("An exception can only be fatal or nonfatal, not {0}", fatalityState)); 

            }
        }
    }

    private static Dictionary<string, exception> registry = new (StringComparer.OrdinalIgnoreCase){
        /*it seems that all regular exceptions are uppercase
        All exceptions are false and checked for by default
        I thought about metaprogramming this block, but any solution would just be as time-consuming.
        All exceptions where it's up to the implementor to make the call(imp) are fatal by default for now.
        See the standard for more information.
        */
        {"EC-ALL", new exception("EC-ALL", false, true, "other")},
        {"EC-ARGUMENT", new exception("EC-ARGUMENT", false, true, "other")},
        {"EC-ARGUMENT-FUNCTION", new exception("EC-ARGUMENT-FUNCTION", false, true, "fatal")},
        {"EC-ARGUMENT-IMP", new exception("EC-ARGUMENT-IMP", false, true, "fatal")},//imp
        {"EC-BOUND", new exception("EC-BOUND", false, true, "other")},
        {"EC-BOUND-FUNC-RET-VALUE", new exception("EC-BOUND-FUNC-RET-VALUE", false, true, "nonfatal")},
        {"EC-BOUND-IMP", new exception("EC-BOUND-IMP", false, true, "fatal")},//imp
        {"EC-BOUND-ODO", new exception("EC-BOUND-ODO", false, true, "fatal")},
        {"EC-BOUND-OVERFLOW", new exception("EC-BOUND-OVERFLOW", false, true, "nonfatal")},
        {"EC-BOUND-PTR", new exception("EC-BOUND-PTR", false, true, "fatal")},
        {"EC-BOUND-REF-MOD", new exception("EC-BOUND-REF-MOD", false, true, "fatal")},
        {"EC-BOUND-SET", new exception("EC-BOUND-SET", false, true, "nonfatal")},
        {"EC-BOUND-SUBSCRIPT", new exception("EC-BOUND-SUBSCRIPT", false, true, "fatal")},
        {"EC-BOUND-TABLE-LIMIT", new exception("EC-BOUND-TABLE-LIMIT", false, true, "fatal")},
        {"EC-CONTINUE", new exception("EC-CONTINUE", false, true, "other")},
        {"EC-CONTINUE-IMP", new exception("EC-CONTINUE-IMP", false, true, "fatal")}, //imp 
        {"EC-CONTINUE-LESS-THAN-ZERO", new exception("EC-CONTINUE-LESS-THAN-ZERO", false, true, "nonfatal")},
        {"EC-DATA", new exception("EC-DATA", false, true, "other")},
        {"EC-DATA-CONVERSION", new exception("EC-DATA-CONVERSION", false, true, "nonfatal")},
        {"EC-DATA-IMP", new exception("EC-DATA-IMP", false, true, "fatal")}, //imp
        {"EC-DATA-INCOMPATIBLE", new exception("EC-DATA-INCOMPATIBLE", false, true, "fatal")},
        {"EC-DATA-NOT-FINITE", new exception("EC-DATA-NOT-FINITE", false, true, "fatal")},
        {"EC-DATA-OVERFLOW", new exception("EC-DATA-OVERFLOW", false, true, "fatal")},
        {"EC-DATA-PTR-NULL", new exception("EC-DATA-PTR-NULL", false, true, "fatal")},
        {"EC-EXTERNAL", new exception("EC-EXTERNAL", false, true, "other")},
        {"EC-EXTERNAL-DATA-MISMATCH", new exception("EC-EXTERNAL-DATA-MISMATCH", false, true, "fatal")},
        {"EC-EXTERNAL-FILE-MISMATCH", new exception("EC-EXTERNAL-FILE-MISMATCH", false, true, "fatal")},
        {"EC-EXTERNAL-FORMAT-CONFLICT", new exception("EC-EXTERNAL-FORMAT-CONFLICT", false, true, "fatal")},
        {"EC-EXTERNAL-IMP", new exception("EC-EXTERNAL-IMP", false, true, "false")}, //imp
        {"EC-FLOW", new exception("EC-FLOW", false, true, "other")},
        {"EC-FLOW-APPLY-COMMIT", new exception("EC-FLOW-APPLY-COMMIT", false, true, "fatal")},
        {"EC-FLOW-COMMIT", new exception("EC-FLOW-COMMIT", false, true, "fatal")},
        {"EC-FLOW-GLOBAL-EXIT", new exception("EC-FLOW-GLOBAL-EXIT", false, true, "fatal")},
        {"EC-FLOW-GLOBAL-GOBACK", new exception("EC-FLOW-GLOBAL-GOBACK", false, true, "fatal")},
        {"EC-FLOW-IMP", new exception("EC-FLOW-IMP", false, true, "fatal")}, //imp
        {"EC-FLOW-RELEASE", new exception("EC-FLOW-RELEASE", false, true, "fatal")},
        {"EC-FLOW-REPORT", new exception("EC-FLOW-REPORT", false, true, "fatal")},
        {"EC-FLOW-RETURN", new exception("EC-FLOW-RETURN", false, true, "fatal")},
        {"EC-FLOW-ROLLBACK", new exception("EC-FLOW-ROLLBACK", false, true, "fatal")},
        {"EC-FLOW-SEARCH", new exception("EC-FLOW-SEARCH", false, true, "fatal")},
        {"EC-FLOW-USE", new exception("EC-FLOW-USE", false, true, "fatal")},
        {"EC-FUNCTION", new exception("EC-FUNCTION", false, true, "other")},
        {"EC-FUNCTION-ARG-OMITTED", new exception("EC-FUNCTION-ARG-OMITTED", false, true, "other")},
        {"EC-FUNCTION-IMP", new exception("EC-FUNCTION-IMP", false, true, "fatal")}, //imp
        {"EC-FUNCTION-NOT-FOUND", new exception("EC-FUNCTION-NOT-FOUND", false, true, "fatal")},
        {"EC-FUNCTION-PTR-INVALID", new exception("EC-FUNCTION-PTR-INVALID", false, true, "fatal")},
        {"EC-FUNCTION-PTR-NULL", new exception("EC-FUNCTION-PTR-NULL", false, true, "fatal")},
        {"EC-I-O", new exception("EC-I-O", false, true, "other")},
        {"EC-I-O-AT-END", new exception("EC-I-O-AT-END", false, true, "nonfatal")},
        {"EC-I-O-EOP", new exception("EC-I-O-EOP", false, true, "nonfatal")},
        {"EC-I-O-EOP-OVERFLOW", new exception("EC-I-O-EOP-OVERFLOW", false, true, "nonfatal")},
        {"EC-I-O-FILE-SHARING", new exception("EC-I-O-FILE-SHARING", false, true, "nonfatal")},
        {"EC-I-O-IMP", new exception("EC-I-O-IMP", false, true, "fatal")}, //imp
        {"EC-I-O-INVALID-KEY", new exception("EC-I-O-INVALID-KEY", false, true, "nonfatal")},
        {"EC-I-O-LINAGE", new exception("EC-I-O-LINAGE", false, true, "fatal")},
        {"EC-I-O-LOGIC-ERROR", new exception("EC-I-O-LOGIC-ERROR", false, true, "fatal")},
        {"EC-I-O-PERMANENT-ERROR", new exception("EC-I-O-PERMANENT-ERROR", false, true, "fatal")},
        {"EC-I-O-RECORD-CONTENT", new exception("EC-I-O-RECORD-CONTENT", false, true, "false")},
        {"EC-I-O-RECORD-OPERATION", new exception("EC-I-O-RECORD-OPERATION", false, true, "nonfatal")},
        {"EC-I-O-WARNING", new exception("EC-I-O-WARNING", false, true, "nonfatal")},
        {"EC-IMP", new exception("EC-IMP", false, true, "other")},

        {"EC-IMP-suffix", new exception("EC-IMP-suffix", false, true, "other")},
        /*
        This entry above is unique, so this comment is to bring attention to this situation. Accoridng to this standard, 
        the "suffix" part is defined by the implementor as a custom level-3 exception with the level-2 naturally being 
        "EC-IMP". So there needs to be work done to decide what this exception should be.
        */

        {"EC-LOCALE", new exception("EC-LOCALE", false, true, "other")},
        {"EC-LOCALE-IMP", new exception("EC-LOCALE-IMP", false, true, "fatal")}, //imp
        {"EC-LOCALE-INCOMPATIBLE", new exception("EC-LOCALE-INCOMPATIBLE", false, true, "fatal")},
        {"EC-LOCALE-INVALID", new exception("EC-LOCALE-INVALID", false, true, "fatal")},
        {"EC-LOCALE-INVALID-PTR", new exception("EC-LOCALE-INVALID-PTR", false, true, "fatal")},
        {"EC-LOCALE-MISSING", new exception("EC-LOCALE-MISSING", false, true, "fatal")},
        {"EC-LOCALE-SIZE", new exception("EC-LOCALE-SIZE", false, true, "fatal")},
        {"EC-MCS", new exception("EC-MCS", false, true, "other")},
        {"EC-MCS-ABNORMAL-TERMINATION", new exception("EC-MCS-ABNORMAL-TERMINATION", false, true, "nonfatal")},
        {"EC-MCS-IMP", new exception("EC-MCS-IMP", false, true, "fatal")}, //imp
        {"EC-MCS-INVALID-TAG", new exception("EC-MCS-INVALID-TAG", false, true, "nonfatal")},
        {"EC-MCS-MESSAGE-LENGTH", new exception("EC-MCS-MESSAGE-LENGTH", false, true, "nonfatal")},
        {"EC-MCS-NO-REQUESTER", new exception("EC-MCS-NO-REQUESTER", false, true, "nonfatal")},
        {"EC-MCS-NO-SERVER", new exception("EC-MCS-NO-SERVER", false, true, "nonfatal")},
        {"EC-MCS-NORMAL-TERMINATION", new exception("EC-MCS-NORMAL-TERMINATION", false, true, "nonfatal")},
        {"EC-MCS-REQUESTOR-FAILED", new exception("EC-MCS-REQUESTOR-FAILED", false, true, "nonfatal")},
        {"EC-OO", new exception("EC-OO", false, true, "other")},
        {"EC-OO-ARG-OMITTED", new exception("EC-OO-ARG-OMITTED", false, true, "fatal")},
        {"EC-OO-CONFORMANCE", new exception("EC-OO-CONFORMANCE", false, true, "fatal")},
        {"EC-OO-EXCEPTION", new exception("EC-OO-EXCEPTION", false, true, "fatal")},
        {"EC-OO-IMP", new exception("EC-OO-IMP", false, true, "fatal")}, //imp
        {"EC-OO-METHOD", new exception("EC-OO-METHOD", false, true, "fatal")},
        {"EC-OO-NULL", new exception("EC-OO-NULL", false, true, "fatal")},
        {"EC-OO-RESOURCE", new exception("EC-OO-RESOURCE", false, true, "fatal")},
        {"EC-OO-UNIVERSAL", new exception("EC-OO-UNIVERSAL", false, true, "fatal")},
        {"EC-ORDER", new exception("EC-ORDER", false, true, "other")},
        {"EC-ORDER-IMP", new exception("EC-ORDER-IMP", false, true, "fatal")}, //imp
        {"EC-ORDER-NOT-SUPPORTED", new exception("EC-ORDER-NOT-SUPPORTED", false, true, "fatal")},
        {"EC-OVERFLOW", new exception("EC-OVERFLOW", false, true, "other")},
        {"EC-OVERFLOW-IMP", new exception("EC-OVERFLOW-IMP", false, true, "fatal")}, //imp
        {"EC-OVERFLOW-STRING", new exception("EC-OVERFLOW-STRING", false, true, "nonfatal")},
        {"EC-OVERFLOW-UNSTRING", new exception("EC-OVERFLOW-UNSTRING", false, true, "nonfatal")},
        {"EC-PROGRAM", new exception("EC-PROGRAM", false, true, "other")},
        {"EC-PROGRAM-ARG-MISMATCH", new exception("EC-PROGRAM-ARG-MISMATCH", false, true, "fatal")},
        {"EC-PROGRAM-ARG-OMITTED", new exception("EC-PROGRAM-ARG-OMITTED", false, true, "fatal")},
        {"EC-PROGRAM-CANCEL-ACTIVE", new exception("EC-PROGRAM-CANCEL-ACTIVE", false, true, "fatal")},
        {"EC-PROGRAM-IMP", new exception("EC-PROGRAM-IMP", false, true, "fatal")}, //imp
        {"EC-PROGRAM-NOT-FOUND", new exception("EC-PROGRAM-NOT-FOUND", false, true, "fatal")}, //imp
        {"EC-PROGRAM-PTR-NULL", new exception("EC-PROGRAM-PTR-NULL", false, true, "fatal")},
        {"EC-PROGRAM-RECURSIVE-CALL", new exception("EC-PROGRAM-RECURSIVE-CALL", false, true, "fatal")},
        {"EC-PROGRAM-RESOURCES", new exception("EC-PROGRAM-RESOURCES", false, true, "fatal")},
        {"EC-RAISING", new exception("EC-RAISING", false, true, "other")},
        {"EC-RAISING-IMP", new exception("EC-RAISING-IMP", false, true, "fatal")}, //imp
        {"EC-RAISING-NOT-SPECIFIED", new exception("EC-RAISING-NOT-SPECIFIED", false, true, "fatal")},
        {"EC-RANGE", new exception("EC-RANGE", false, true, "other")},
        {"EC-RANGE-IMP", new exception("EC-RANGE-IMP", false, true, "fatal")}, //imp
        {"EC-RANGE-INDEX", new exception("EC-RANGE-INDEX", false, true, "fatal")},
        {"EC-RANGE-INSPECT-SIZE", new exception("EC-RANGE-INSPECT-SIZE", false, true, "fatal")},
        {"EC-RANGE-INVALID", new exception("EC-RANGE-INVALID", false, true, "nonfatal")},
        {"EC-RANGE-PERFORM-VARYING", new exception("EC-RANGE-PERFORM-VARYING", false, true, "fatal")},
        {"EC-RANGE-PTR", new exception("EC-RANGE-PTR", false, true, "fatal")},
        {"EC-RANGE-SEARCH-INDEX", new exception("EC-RANGE-SEARCH-INDEX", false, true, "nonfatal")},
        {"EC-RANGE-SEARCH-NO-MATCH", new exception("EC-RANGE-SEARCH-NO-MATCH", false, true, "nonfatal")},
        {"EC-REPORT", new exception("EC-REPORT", false, true, "other")},
        {"EC-REPORT-ACTIVE", new exception("EC-REPORT-ACTIVE", false, true, "fatal")},
        {"EC-REPORT-COLUMN-OVERLAP", new exception("EC-REPORT-COLUMN-OVERLAP", false, true, "nonfatal")},
        {"EC-REPORT-FILE-MODE", new exception("EC-REPORT-FILE-MODE", false, true, "fatal")},
        {"EC-REPORT-IMP", new exception("EC-REPORT-IMP", false, true, "fatal")}, //imp
        {"EC-REPORT-INACTIVE", new exception("EC-REPORT-INACTIVE", false, true, "fatal")},
        {"EC-REPORT-LINE-OVERLAP", new exception("EC-REPORT-LINE-OVERLAP", false, true, "nonfatal")},
        {"EC-REPORT-NOT-TERMINATED", new exception("EC-REPORT-NOT-TERMINATED", false, true, "nonfatal")},
        {"EC-REPORT-PAGE-LIMIT", new exception("EC-REPORT-PAGE-LIMIT", false, true, "nonfatal")},
        {"EC-REPORT-PAGE-WIDTH", new exception("EC-REPORT-PAGE-WIDTH", false, true, "nonfatal")},
        {"EC-REPORT-SUM-SIZE", new exception("EC-REPORT-SUM-SIZE", false, true, "fatal")},
        {"EC-REPORT-VARYING", new exception("EC-REPORT-VARYING", false, true, "fatal")},
        {"EC-SCREEN", new exception("EC-SCREEN", false, true, "other")},
        {"EC-SCREEN-FIELD-OVERLAP", new exception("EC-SCREEN-FIELD-OVERLAP", false, true, "nonfatal")}, 
        {"EC-SCREEN-IMP", new exception("EC-SCREEN-IMP", false, true, "fatal")}, //imp
        {"EC-SCREEN-ITEM-TRUNCATED", new exception("EC-SCREEN-ITEM-TRUNCATED", false, true, "nonfatal")},
        {"EC-SCREEN-LINE-NUMBER", new exception("EC-SCREEN-LINE-NUMBER", false, true, "nonfatal")},
        {"EC-SCREEN-STARTING-COLUMN", new exception("EC-SCREEN-STARTING-COLUMN", false, true, "nonfatal")},
        {"EC-SIZE", new exception("EC-SIZE", false, true, "other")},
        {"EC-SIZE-ADDRESS", new exception("EC-SIZE-ADDRESS", false, true, "fatal")},
        {"EC-SIZE-EXPONENTIATION", new exception("EC-SIZE-EXPONENTIATION", false, true, "fatal")},
        {"EC-SIZE-IMP", new exception("EC-SIZE-IMP", false, true, "fatal")}, //imp
        {"EC-SIZE-OVERFLOW", new exception("EC-SIZE-OVERFLOW", false, true, "fatal")},
        {"EC-SIZE-TRUNCATION", new exception("EC-SIZE-TRUNCATION", false, true, "fatal")},
        {"EC-SIZE-UNDERFLOW", new exception("EC-SIZE-UNDERFLOW", false, true, "fatal")},
        {"EC-SIZE-ZERO-DIVIDE", new exception("EC-SIZE-ZERO-DIVIDE", false, true, "fatal")},
        {"EC-SORT-MERGE", new exception("EC-SORT-MERGE", false, true, "other")},
        {"EC-SORT-MERGE-ACTIVE", new exception("EC-SORT-MERGE-ACTIVE", false, true, "fatal")},
        {"EC-SORT-MERGE-FILE-OPEN", new exception("EC-SORT-MERGE-FILE-OPEN", false, true, "fatal")},
        {"EC-SORT-MERGE-IMP", new exception("EC-SORT-MERGE-IMP", false, true, "fatal")}, //imp
        {"EC-SORT-MERGE-RELEASE", new exception("EC-SORT-MERGE-RELEASE", false, true, "fatal")},
        {"EC-SORT-MERGE-RETURN", new exception("EC-SORT-MERGE-RETURN", false, true, "fatal")},
        {"EC-SORT-MERGE-SEQUENCE", new exception("EC-SORT-MERGE-SEQUENCE", false, true, "fatal")},
        {"EC-STORAGE", new exception("EC-STORAGE", false, true, "other")},
        {"EC-STORAGE-IMP", new exception("EC-STORAGE-IMP", false, true, "fatal")}, //imp
        {"EC-STORAGE-NOT-ALLOC", new exception("EC-STORAGE-NOT-ALLOC", false, true, "nonfatal")},
        {"EC-STORAGE-NOT-AVAIL", new exception("EC-STORAGE-NOT-AVAIL", false, true, "nonfatal")},
        {"EC-USER", new exception("EC-USER", false, true, "other")}, //user-defined

        {"EC-USER-suffix", new exception("EC-USER-suffix", false, true, "nonfatal")}, 
        //similar to the previous suffix exception, but this time the user decides

        {"EC-VALIDATE", new exception("EC-VALIDATE", false, true, "other")},
        {"EC-VALIDATE-CONTENT", new exception("EC-VALIDATE-CONTENT", false, true, "nonfatal")},
        {"EC-VALIDATE-FORMAT", new exception("EC-VALIDATE-FORMAT", false, true, "nonfatal")},
        {"EC-VALIDATE-IMP", new exception("EC-VALIDATE-IMP", false, true, "fatal")}, //imp
        {"EC-VALIDATE-RELATION", new exception("EC-VALIDATE-RELATION", false, true, "nonfatal")},
        {"EC-VALIDATE-VARYING", new exception("EC-VALIDATE-VARYING", false, true, "fatal")}
    };
}