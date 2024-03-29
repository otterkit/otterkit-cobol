using System.Text;

namespace Otterkit;

public static partial class Tools
{
    private const string ExceptionIndexPath = "../Libraries/Otterkit.Runtime/src/Generated/ExceptionIndex.cs";

    public static void GenerateExceptionIndex()
    {
        var builder = new StringBuilder();

        builder.AppendLine("// This file is auto-generated by Otterkit, do not edit manually.");
        builder.AppendLine("// Compiler Tool: src/Tools/GenerateExceptionIndex.cs");
        builder.AppendLine("namespace Otterkit.Runtime;\n");
        builder.AppendLine("public static partial class RuntimeHelpers");
        builder.AppendLine("{");
        builder.AppendLine("    // This method performs a case-insensitive lookup of an exception name.");
        builder.AppendLine("    // It returns the index of the exception in the exception table.");
        builder.AppendLine("    public static int FetchExceptionIndex(ReadOnlySpan<byte> name)");
        builder.AppendLine("    {");
        builder.AppendLine("        return name switch");
        builder.AppendLine("        {");

        GenerateSwitchLookup(
            builder,
            "EC-ALL",
            "EC-ARGUMENT",
            "EC-ARGUMENT-FUNCTION",
            "EC-ARGUMENT-IMP",
            "EC-BOUND",
            "EC-BOUND-FUNC-RET-VALUE",
            "EC-BOUND-IMP",
            "EC-BOUND-ODO",
            "EC-BOUND-OVERFLOW",
            "EC-BOUND-PTR",
            "EC-BOUND-REF-MOD",
            "EC-BOUND-SET",
            "EC-BOUND-SUBSCRIPT",
            "EC-BOUND-TABLE-LIMIT",
            "EC-CONTINUE",
            "EC-CONTINUE-IMP",
            "EC-CONTINUE-LESS-THAN-ZERO",
            "EC-DATA",
            "EC-DATA-CONVERSION",
            "EC-DATA-IMP",
            "EC-DATA-INCOMPATIBLE",
            "EC-DATA-NOT-FINITE",
            "EC-DATA-OVERFLOW",
            "EC-DATA-PTR-NULL",
            "EC-EXTERNAL",
            "EC-EXTERNAL-DATA-MISMATCH",
            "EC-EXTERNAL-FILE-MISMATCH",
            "EC-EXTERNAL-FORMAT-CONFLICT",
            "EC-EXTERNAL-IMP",
            "EC-FLOW",
            "EC-FLOW-APPLY-COMMIT",
            "EC-FLOW-COMMIT",
            "EC-FLOW-GLOBAL-EXIT",
            "EC-FLOW-GLOBAL-GOBACK",
            "EC-FLOW-IMP",
            "EC-FLOW-RELEASE",
            "EC-FLOW-REPORT",
            "EC-FLOW-RETURN",
            "EC-FLOW-ROLLBACK",
            "EC-FLOW-SEARCH",
            "EC-FLOW-USE",
            "EC-FUNCTION",
            "EC-FUNCTION-ARG-OMITTED",
            "EC-FUNCTION-IMP",
            "EC-FUNCTION-NOT-FOUND",
            "EC-FUNCTION-PTR-INVALID",
            "EC-FUNCTION-PTR-NULL",
            "EC-I-O",
            "EC-I-O-AT-END",
            "EC-I-O-EOP",
            "EC-I-O-EOP-OVERFLOW",
            "EC-I-O-FILE-SHARING",
            "EC-I-O-IMP",
            "EC-I-O-INVALID-KEY",
            "EC-I-O-LINAGE",
            "EC-I-O-LOGIC-ERROR",
            "EC-I-O-PERMANENT-ERROR",
            "EC-I-O-RECORD-CONTENT",
            "EC-I-O-RECORD-OPERATION",
            "EC-I-O-WARNING",
            "EC-IMP",
            "EC-IMP-suffix",
            "EC-LOCALE",
            "EC-LOCALE-IMP",
            "EC-LOCALE-INCOMPATIBLE",
            "EC-LOCALE-INVALID",
            "EC-LOCALE-INVALID-PTR",
            "EC-LOCALE-MISSING",
            "EC-LOCALE-SIZE",
            "EC-MCS",
            "EC-MCS-ABNORMAL-TERMINATION",
            "EC-MCS-IMP",
            "EC-MCS-INVALID-TAG",
            "EC-MCS-MESSAGE-LENGTH",
            "EC-MCS-NO-REQUESTER",
            "EC-MCS-NO-SERVER",
            "EC-MCS-NORMAL-TERMINATION",
            "EC-MCS-REQUESTOR-FAILED",
            "EC-OO",
            "EC-OO-ARG-OMITTED",
            "EC-OO-CONFORMANCE",
            "EC-OO-EXCEPTION",
            "EC-OO-IMP",
            "EC-OO-METHOD",
            "EC-OO-NULL",
            "EC-OO-RESOURCE",
            "EC-OO-UNIVERSAL",
            "EC-ORDER",
            "EC-ORDER-IMP",
            "EC-ORDER-NOT-SUPPORTED",
            "EC-OVERFLOW",
            "EC-OVERFLOW-IMP",
            "EC-OVERFLOW-STRING",
            "EC-OVERFLOW-UNSTRING",
            "EC-PROGRAM",
            "EC-PROGRAM-ARG-MISMATCH",
            "EC-PROGRAM-ARG-OMITTED",
            "EC-PROGRAM-CANCEL-ACTIVE",
            "EC-PROGRAM-IMP",
            "EC-PROGRAM-NOT-FOUND",
            "EC-PROGRAM-PTR-NULL",
            "EC-PROGRAM-RECURSIVE-CALL",
            "EC-PROGRAM-RESOURCES",
            "EC-RAISING",
            "EC-RAISING-IMP",
            "EC-RAISING-NOT-SPECIFIED",
            "EC-RANGE",
            "EC-RANGE-IMP",
            "EC-RANGE-INDEX",
            "EC-RANGE-INSPECT-SIZE",
            "EC-RANGE-INVALID",
            "EC-RANGE-PERFORM-VARYING",
            "EC-RANGE-PTR",
            "EC-RANGE-SEARCH-INDEX",
            "EC-RANGE-SEARCH-NO-MATCH",
            "EC-REPORT",
            "EC-REPORT-ACTIVE",
            "EC-REPORT-COLUMN-OVERLAP",
            "EC-REPORT-FILE-MODE",
            "EC-REPORT-IMP",
            "EC-REPORT-INACTIVE",
            "EC-REPORT-LINE-OVERLAP",
            "EC-REPORT-NOT-TERMINATED",
            "EC-REPORT-PAGE-LIMIT",
            "EC-REPORT-PAGE-WIDTH",
            "EC-REPORT-SUM-SIZE",
            "EC-REPORT-VARYING",
            "EC-SCREEN",
            "EC-SCREEN-FIELD-OVERLAP",
            "EC-SCREEN-IMP",
            "EC-SCREEN-ITEM-TRUNCATED",
            "EC-SCREEN-LINE-NUMBER",
            "EC-SCREEN-STARTING-COLUMN",
            "EC-SIZE",
            "EC-SIZE-ADDRESS",
            "EC-SIZE-EXPONENTIATION",
            "EC-SIZE-IMP",
            "EC-SIZE-OVERFLOW",
            "EC-SIZE-TRUNCATION",
            "EC-SIZE-UNDERFLOW",
            "EC-SIZE-ZERO-DIVIDE",
            "EC-SORT-MERGE",
            "EC-SORT-MERGE-ACTIVE",
            "EC-SORT-MERGE-FILE-OPEN",
            "EC-SORT-MERGE-IMP",
            "EC-SORT-MERGE-RELEASE",
            "EC-SORT-MERGE-RETURN",
            "EC-SORT-MERGE-SEQUENCE",
            "EC-STORAGE",
            "EC-STORAGE-IMP",
            "EC-STORAGE-NOT-ALLOC",
            "EC-STORAGE-NOT-AVAIL",
            "EC-USER",
            "EC-USER-suffix",
            "EC-VALIDATE",
            "EC-VALIDATE-CONTENT",
            "EC-VALIDATE-FORMAT",
            "EC-VALIDATE-IMP",
            "EC-VALIDATE-RELATION",
            "EC-VALIDATE-VARYING"
        );

        builder.AppendLine("            _ => -5");
        builder.AppendLine("        };");
        builder.AppendLine("    }");
        builder.AppendLine("}");

        File.WriteAllText(ExceptionIndexPath, builder.ToString());
    }
}
