namespace Otterkit.Runtime;

// Implementing COBOL default/regular exceptions as a singleton containing a dictionary of exception status indicators
public static class ExceptionRegistry
{
    public static CobolException LastException { get; set; }
    public static readonly byte[] LastName = new byte[64];

    public static ref CobolException FetchException(ReadOnlySpan<byte> name)
    {
        var index = RuntimeHelpers.FetchExceptionIndex(name);

        ref var exception = ref ExceptionLookup[index];

        return ref exception;
    }

    private static CobolException ChangeStatus(ReadOnlySpan<byte> name, bool status)
    {
        ref var exception = ref FetchException(name);

        exception = new(status, exception.Checked, exception.Severity);

        return exception;
    }

    private static void ChangeChecked(ReadOnlySpan<byte> name, bool _checked)
    {
        ref var exception = ref FetchException(name);

        exception = new(exception.Active, _checked, exception.Severity);
    }

    private static bool IsExceptionChecked(ReadOnlySpan<byte> name)
    {
        ref var exception = ref FetchException(name);

        return exception.Checked;
    }

    public static void CheckOn(ReadOnlySpan<byte> name)
    {
        ChangeChecked(name, true);
    }

    public static void CheckOff(ReadOnlySpan<byte> name)
    {
        ChangeChecked(name, false);
    }

    public static void ActivateException(ReadOnlySpan<byte> name)
    {
        LastException = ChangeStatus(name, true);

        var lastName = LastName.AsSpan();

        lastName.Fill(0);

        name.CopyTo(lastName);
    }

    public static void DeactivateException(ReadOnlySpan<byte> name)
    {
        ChangeStatus(name, false);
    }

    /*
    As of writing this comment, activateException() and deactivateException() are sort of black boxes from an outside view: they perform what you think they would do, but do not
    care about if the exception was already on or off. Addtionally, activateException() only activates if the exception is being actively checked. This probably fits the
    behavior of the standard, but is this a good abstraction?

    Note from KTSnowy: Exceptions should be activated regardless of their checked status, 
    this is because exceptions can be checked with an ON EXCEPTION or ON OVERFLOW clause without their checked status being on.
    But automatic checking without an ON EXCEPTION clause only happens if the exception's check status is on.
    */

    public readonly static CobolException[] ExceptionLookup = new CobolException[]
    {
        /*
        It seems that all regular exceptions are uppercase
        All exceptions are false and checked for by default
        I thought about metaprogramming this block, but any solution would just be as time-consuming.
        All exceptions where it's up to the implementor to make the call(imp) are fatal by default for now.
        See the standard for more information.
        */

        /*"EC-ALL",                      */new (false, true, Severity.Other),
        /*"EC-ARGUMENT",                 */new (false, true, Severity.Other),
        /*"EC-ARGUMENT-FUNCTION",        */new (false, true, Severity.Fatal),
        /*"EC-ARGUMENT-IMP",             */new (false, true, Severity.Fatal),
        /*"EC-BOUND",                    */new (false, true, Severity.Other),
        /*"EC-BOUND-FUNC-RET-VALUE",     */new (false, true, Severity.NonFatal),
        /*"EC-BOUND-IMP",                */new (false, true, Severity.Fatal),
        /*"EC-BOUND-ODO",                */new (false, true, Severity.Fatal),
        /*"EC-BOUND-OVERFLOW",           */new (false, true, Severity.NonFatal),
        /*"EC-BOUND-PTR",                */new (false, true, Severity.Fatal),
        /*"EC-BOUND-REF-MOD",            */new (false, true, Severity.Fatal),
        /*"EC-BOUND-SET",                */new (false, true, Severity.NonFatal),
        /*"EC-BOUND-SUBSCRIPT",          */new (false, true, Severity.Fatal),
        /*"EC-BOUND-TABLE-LIMIT",        */new (false, true, Severity.Fatal),
        /*"EC-CONTINUE",                 */new (false, true, Severity.Other),
        /*"EC-CONTINUE-IMP",             */new (false, true, Severity.Fatal),
        /*"EC-CONTINUE-LESS-THAN-ZERO",  */new (false, true, Severity.NonFatal),
        /*"EC-DATA",                     */new (false, true, Severity.Other),
        /*"EC-DATA-CONVERSION",          */new (false, true, Severity.NonFatal),
        /*"EC-DATA-IMP",                 */new (false, true, Severity.Fatal),
        /*"EC-DATA-INCOMPATIBLE",        */new (false, true, Severity.Fatal),
        /*"EC-DATA-NOT-FINITE",          */new (false, true, Severity.Fatal),
        /*"EC-DATA-OVERFLOW",            */new (false, true, Severity.Fatal),
        /*"EC-DATA-PTR-NULL",            */new (false, true, Severity.Fatal),
        /*"EC-EXTERNAL",                 */new (false, true, Severity.Other),
        /*"EC-EXTERNAL-DATA-MISMATCH",   */new (false, true, Severity.Fatal),
        /*"EC-EXTERNAL-FILE-MISMATCH",   */new (false, true, Severity.Fatal),
        /*"EC-EXTERNAL-FORMAT-CONFLICT", */new (false, true, Severity.Fatal),
        /*"EC-EXTERNAL-IMP",             */new (false, true, Severity.NonFatal),
        /*"EC-FLOW",                     */new (false, true, Severity.Other),
        /*"EC-FLOW-APPLY-COMMIT",        */new (false, true, Severity.Fatal),
        /*"EC-FLOW-COMMIT",              */new (false, true, Severity.Fatal),
        /*"EC-FLOW-GLOBAL-EXIT",         */new (false, true, Severity.Fatal),
        /*"EC-FLOW-GLOBAL-GOBACK",       */new (false, true, Severity.Fatal),
        /*"EC-FLOW-IMP",                 */new (false, true, Severity.Fatal),
        /*"EC-FLOW-RELEASE",             */new (false, true, Severity.Fatal),
        /*"EC-FLOW-REPORT",              */new (false, true, Severity.Fatal),
        /*"EC-FLOW-RETURN",              */new (false, true, Severity.Fatal),
        /*"EC-FLOW-ROLLBACK",            */new (false, true, Severity.Fatal),
        /*"EC-FLOW-SEARCH",              */new (false, true, Severity.Fatal),
        /*"EC-FLOW-USE",                 */new (false, true, Severity.Fatal),
        /*"EC-FUNCTION",                 */new (false, true, Severity.Other),
        /*"EC-FUNCTION-ARG-OMITTED",     */new (false, true, Severity.Other),
        /*"EC-FUNCTION-IMP",             */new (false, true, Severity.Fatal),
        /*"EC-FUNCTION-NOT-FOUND",       */new (false, true, Severity.Fatal),
        /*"EC-FUNCTION-PTR-INVALID",     */new (false, true, Severity.Fatal),
        /*"EC-FUNCTION-PTR-NULL",        */new (false, true, Severity.Fatal),
        /*"EC-I-O",                      */new (false, true, Severity.Other),
        /*"EC-I-O-AT-END",               */new (false, true, Severity.NonFatal),
        /*"EC-I-O-EOP",                  */new (false, true, Severity.NonFatal),
        /*"EC-I-O-EOP-OVERFLOW",         */new (false, true, Severity.NonFatal),
        /*"EC-I-O-FILE-SHARING",         */new (false, true, Severity.NonFatal),
        /*"EC-I-O-IMP",                  */new (false, true, Severity.Fatal),
        /*"EC-I-O-INVALID-KEY",          */new (false, true, Severity.NonFatal),
        /*"EC-I-O-LINAGE",               */new (false, true, Severity.Fatal),
        /*"EC-I-O-LOGIC-ERROR",          */new (false, true, Severity.Fatal),
        /*"EC-I-O-PERMANENT-ERROR",      */new (false, true, Severity.Fatal),
        /*"EC-I-O-RECORD-CONTENT",       */new (false, true, Severity.NonFatal),
        /*"EC-I-O-RECORD-OPERATION",     */new (false, true, Severity.NonFatal),
        /*"EC-I-O-WARNING",              */new (false, true, Severity.NonFatal),
        /*"EC-IMP",                      */new (false, true, Severity.Other),

        /*
        This entry is unique, so this comment is to bring attention to this situation. Accoridng to the standard, 
        the "suffix" part is defined by the implementor as a custom level-3 exception with the level-2 naturally being 
        "EC-IMP". So there needs to be work done to decide what this exception should be.
        */

        /*"EC-IMP-suffix",               */new (false, true, Severity.Other),
        /*"EC-LOCALE",                   */new (false, true, Severity.Other),
        /*"EC-LOCALE-IMP",               */new (false, true, Severity.Fatal),
        /*"EC-LOCALE-INCOMPATIBLE",      */new (false, true, Severity.Fatal),
        /*"EC-LOCALE-INVALID",           */new (false, true, Severity.Fatal),
        /*"EC-LOCALE-INVALID-PTR",       */new (false, true, Severity.Fatal),
        /*"EC-LOCALE-MISSING",           */new (false, true, Severity.Fatal),
        /*"EC-LOCALE-SIZE",              */new (false, true, Severity.Fatal),
        /*"EC-MCS",                      */new (false, true, Severity.Other),
        /*"EC-MCS-ABNORMAL-TERMINATION", */new (false, true, Severity.NonFatal),
        /*"EC-MCS-IMP",                  */new (false, true, Severity.Fatal),
        /*"EC-MCS-INVALID-TAG",          */new (false, true, Severity.NonFatal),
        /*"EC-MCS-MESSAGE-LENGTH",       */new (false, true, Severity.NonFatal),
        /*"EC-MCS-NO-REQUESTER",         */new (false, true, Severity.NonFatal),
        /*"EC-MCS-NO-SERVER",            */new (false, true, Severity.NonFatal),
        /*"EC-MCS-NORMAL-TERMINATION",   */new (false, true, Severity.NonFatal),
        /*"EC-MCS-REQUESTOR-FAILED",     */new (false, true, Severity.NonFatal),
        /*"EC-OO",                       */new (false, true, Severity.Other),
        /*"EC-OO-ARG-OMITTED",           */new (false, true, Severity.Fatal),
        /*"EC-OO-CONFORMANCE",           */new (false, true, Severity.Fatal),
        /*"EC-OO-EXCEPTION",             */new (false, true, Severity.Fatal),
        /*"EC-OO-IMP",                   */new (false, true, Severity.Fatal),
        /*"EC-OO-METHOD",                */new (false, true, Severity.Fatal),
        /*"EC-OO-NULL",                  */new (false, true, Severity.Fatal),
        /*"EC-OO-RESOURCE",              */new (false, true, Severity.Fatal),
        /*"EC-OO-UNIVERSAL",             */new (false, true, Severity.Fatal),
        /*"EC-ORDER",                    */new (false, true, Severity.Other),
        /*"EC-ORDER-IMP",                */new (false, true, Severity.Fatal),
        /*"EC-ORDER-NOT-SUPPORTED",      */new (false, true, Severity.Fatal),
        /*"EC-OVERFLOW",                 */new (false, true, Severity.Other),
        /*"EC-OVERFLOW-IMP",             */new (false, true, Severity.Fatal),
        /*"EC-OVERFLOW-STRING",          */new (false, true, Severity.NonFatal),
        /*"EC-OVERFLOW-UNSTRING",        */new (false, true, Severity.NonFatal),
        /*"EC-PROGRAM",                  */new (false, true, Severity.Other),
        /*"EC-PROGRAM-ARG-MISMATCH",     */new (false, true, Severity.Fatal),
        /*"EC-PROGRAM-ARG-OMITTED",      */new (false, true, Severity.Fatal),
        /*"EC-PROGRAM-CANCEL-ACTIVE",    */new (false, true, Severity.Fatal),
        /*"EC-PROGRAM-IMP",              */new (false, true, Severity.Fatal),
        /*"EC-PROGRAM-NOT-FOUND",        */new (false, true, Severity.Fatal),
        /*"EC-PROGRAM-PTR-NULL",         */new (false, true, Severity.Fatal),
        /*"EC-PROGRAM-RECURSIVE-CALL",   */new (false, true, Severity.Fatal),
        /*"EC-PROGRAM-RESOURCES",        */new (false, true, Severity.Fatal),
        /*"EC-RAISING",                  */new (false, true, Severity.Other),
        /*"EC-RAISING-IMP",              */new (false, true, Severity.Fatal),
        /*"EC-RAISING-NOT-SPECIFIED",    */new (false, true, Severity.Fatal),
        /*"EC-RANGE",                    */new (false, true, Severity.Other),
        /*"EC-RANGE-IMP",                */new (false, true, Severity.Fatal),
        /*"EC-RANGE-INDEX",              */new (false, true, Severity.Fatal),
        /*"EC-RANGE-INSPECT-SIZE",       */new (false, true, Severity.Fatal),
        /*"EC-RANGE-INVALID",            */new (false, true, Severity.NonFatal),
        /*"EC-RANGE-PERFORM-VARYING",    */new (false, true, Severity.Fatal),
        /*"EC-RANGE-PTR",                */new (false, true, Severity.Fatal),
        /*"EC-RANGE-SEARCH-INDEX",       */new (false, true, Severity.NonFatal),
        /*"EC-RANGE-SEARCH-NO-MATCH",    */new (false, true, Severity.NonFatal),
        /*"EC-REPORT",                   */new (false, true, Severity.Other),
        /*"EC-REPORT-ACTIVE",            */new (false, true, Severity.Fatal),
        /*"EC-REPORT-COLUMN-OVERLAP",    */new (false, true, Severity.NonFatal),
        /*"EC-REPORT-FILE-MODE",         */new (false, true, Severity.Fatal),
        /*"EC-REPORT-IMP",               */new (false, true, Severity.Fatal),
        /*"EC-REPORT-INACTIVE",          */new (false, true, Severity.Fatal),
        /*"EC-REPORT-LINE-OVERLAP",      */new (false, true, Severity.NonFatal),
        /*"EC-REPORT-NOT-TERMINATED",    */new (false, true, Severity.NonFatal),
        /*"EC-REPORT-PAGE-LIMIT",        */new (false, true, Severity.NonFatal),
        /*"EC-REPORT-PAGE-WIDTH",        */new (false, true, Severity.NonFatal),
        /*"EC-REPORT-SUM-SIZE",          */new (false, true, Severity.Fatal),
        /*"EC-REPORT-VARYING",           */new (false, true, Severity.Fatal),
        /*"EC-SCREEN",                   */new (false, true, Severity.Other),
        /*"EC-SCREEN-FIELD-OVERLAP",     */new (false, true, Severity.NonFatal),
        /*"EC-SCREEN-IMP",               */new (false, true, Severity.Fatal),
        /*"EC-SCREEN-ITEM-TRUNCATED",    */new (false, true, Severity.NonFatal),
        /*"EC-SCREEN-LINE-NUMBER",       */new (false, true, Severity.NonFatal),
        /*"EC-SCREEN-STARTING-COLUMN",   */new (false, true, Severity.NonFatal),
        /*"EC-SIZE",                     */new (false, true, Severity.Other),
        /*"EC-SIZE-ADDRESS",             */new (false, true, Severity.Fatal),
        /*"EC-SIZE-EXPONENTIATION",      */new (false, true, Severity.Fatal),
        /*"EC-SIZE-IMP",                 */new (false, true, Severity.Fatal),
        /*"EC-SIZE-OVERFLOW",            */new (false, true, Severity.Fatal),
        /*"EC-SIZE-TRUNCATION",          */new (false, true, Severity.Fatal),
        /*"EC-SIZE-UNDERFLOW",           */new (false, true, Severity.Fatal),
        /*"EC-SIZE-ZERO-DIVIDE",         */new (false, true, Severity.Fatal),
        /*"EC-SORT-MERGE",               */new (false, true, Severity.Other),
        /*"EC-SORT-MERGE-ACTIVE",        */new (false, true, Severity.Fatal),
        /*"EC-SORT-MERGE-FILE-OPEN",     */new (false, true, Severity.Fatal),
        /*"EC-SORT-MERGE-IMP",           */new (false, true, Severity.Fatal),
        /*"EC-SORT-MERGE-RELEASE",       */new (false, true, Severity.Fatal),
        /*"EC-SORT-MERGE-RETURN",        */new (false, true, Severity.Fatal),
        /*"EC-SORT-MERGE-SEQUENCE",      */new (false, true, Severity.Fatal),
        /*"EC-STORAGE",                  */new (false, true, Severity.Other),
        /*"EC-STORAGE-IMP",              */new (false, true, Severity.Fatal),
        /*"EC-STORAGE-NOT-ALLOC",        */new (false, true, Severity.NonFatal),
        /*"EC-STORAGE-NOT-AVAIL",        */new (false, true, Severity.NonFatal),
        /*"EC-USER",                     */new (false, true, Severity.Other),
        /*"EC-USER-suffix",              */new (false, true, Severity.NonFatal), // similar to the previous suffix exception, but this time the user decides
        /*"EC-VALIDATE",                 */new (false, true, Severity.Other),
        /*"EC-VALIDATE-CONTENT",         */new (false, true, Severity.NonFatal),
        /*"EC-VALIDATE-FORMAT",          */new (false, true, Severity.NonFatal),
        /*"EC-VALIDATE-IMP",             */new (false, true, Severity.Fatal),
        /*"EC-VALIDATE-RELATION",        */new (false, true, Severity.NonFatal),
        /*"EC-VALIDATE-VARYING",         */new (false, true, Severity.Fatal),
    };
}
