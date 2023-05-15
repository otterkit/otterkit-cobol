using static Otterkit.Types.TokenHandling;

using System.Diagnostics;
using Otterkit.Types;

namespace Otterkit.Analyzers;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static class ProcedureDivision
{
    // Method responsible for parsing the PROCEDURE DIVISION.
    // That includes the user-defined paragraphs, sections and declaratives
    // or when parsing OOP COBOL code, it's responsible for parsing COBOL methods, objects and factories. 
    // It is also responsible for showing appropriate error messages when an error occurs in the PROCEDURE DIVISION.
    public static void ParseProcedural()
    {
        Expected("PROCEDURE");
        Expected("DIVISION");

        CompilerContext.ActiveScope = SourceScope.ProcedureDivision;

        var currentSource = CompilerContext.SourceTypes.Peek();

        if (CurrentEquals("USING"))
        {
            Expected("USING");
            Using();
        }

        if (CompilerContext.SourceTypes.Peek() is UnitKind.Function or UnitKind.FunctionPrototype)
        {
            Expected("RETURNING");
            Returning();
        }
        else if (CurrentEquals("RETURNING"))
        {
            Expected("RETURNING");
            Returning();
        }

        if (!Expected(".", false))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                Division header, missing separator period.
                """)
            .WithSourceLine(Peek(-1), """
                Expected a separator period '. ' after this token
                """)
            .WithNote("""
                Every division header must end with a separator period
                """)
            .CloseError();

            AnchorPoint(TokenContext.IsStatement);
        }

        ProcedureBody();
    }

    // This method is part of the PROCEDURE DIVISION parsing. It's used to parse the "RETURNING" data item specified in
    // the PROCEDURE DIVISION header. It's separate from the previous method because its code is needed more than once.
    // COBOL user-defined functions should always return a data item.

    private static HashSet<string> headerDataNames = new();

    private static void Using()
    {
        while (CurrentEquals("BY REFERENCE VALUE") || CurrentEquals(TokenType.Identifier))
        {
            while (CurrentEquals(TokenType.Identifier) || CurrentEquals("OPTIONAL"))
            {
                var isOptional = false;

                if (CurrentEquals("OPTIONAL"))
                {
                    Expected("OPTIONAL");

                    isOptional = true;
                }

                HandleHeaderDataName(true, isOptional, true);
            }

            if (CurrentEquals("BY") && !PeekEquals(1, "VALUE REFERENCE"))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 128, """
                    Using phrase missing word.
                    """)
                .WithSourceLine(Current(), """
                    Expected 'VALUE' or 'REFERENCE' after this token
                    """)
                .CloseError();

                AnchorPoint(TokenContext.IsStatement, "RETURNING");
            }

            if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && PeekEquals(1, "REFERENCE"))
            {
                Optional("BY");
                Expected("REFERENCE");

                if (!CurrentEquals(TokenType.Identifier) && !PeekEquals(1, TokenType.Identifier))
                {
                    ErrorHandler
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 128, """
                        Using phrase missing identifier.
                        """)
                    .WithSourceLine(Peek(-1), """
                        Must contain at least one data name.
                        """)
                    .CloseError();
                }

                while (CurrentEquals(TokenType.Identifier) || CurrentEquals("OPTIONAL"))
                {
                    var isOptional = false;

                    if (CurrentEquals("OPTIONAL"))
                    {
                        Expected("OPTIONAL");

                        isOptional = true;
                    }

                    HandleHeaderDataName(true, isOptional, true);
                }
            }

            if (CurrentEquals("VALUE") || CurrentEquals("BY") && PeekEquals(1, "VALUE"))
            {
                Optional("BY");
                Expected("VALUE");

                if (CurrentEquals("OPTIONAL"))
                {
                    ErrorHandler
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 128, """
                        Using phrase invalid optional.
                        """)
                    .WithSourceLine(Current(), """
                        Items passed by value cannot be optional.
                        """)
                    .CloseError();
                }

                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 128, """
                        Using phrase missing identifier.
                        """)
                    .WithSourceLine(Peek(-1), """
                        Must contain at least one data name.
                        """)
                    .CloseError();
                }

                while (CurrentEquals(TokenType.Identifier))
                {
                    HandleHeaderDataName(true, false, false);
                }
            }
        }
    }

    private static void Returning()
    {
        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 75, """
                Missing returning data item.
                """)
            .WithSourceLine(Peek(-1), $"""
                Missing returning identifier after this token.
                """)
            .CloseError();
            return;
        }

        HandleHeaderDataName(false);
    }

    // The syntax, semantics and errors for the formal parameters and
    // for the returning item are exactly the same, so we handle it
    // in a sigle method to avoid code duplication.
    private static void HandleHeaderDataName(bool isParameter, bool isOptional = false, bool isByRef = false)
    {
        var dataName = References.LocalName();

        // Don't resolve if it's not a resolution pass.
        if (!CompilerContext.IsResolutionPass) return;

        // Error has been handled in the above Name() call.
        if (!dataName.Exists) return;

        var dataToken = dataName.Unwrap();

        // Uniqueness has not been handled above, 
        // so we handle it here.
        if (!dataName.IsUnique && !CurrentEquals("IN OF"))
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 15, """
                Reference to non-unique identifier.
                """)
            .WithSourceLine(dataToken, """
                Must have a unique name.
                """)
            .WithNote("""
                It must be a unique 77 or 01 level item.
                """)
            .CloseError();
            return;
        }

        // COBOL standard requirement, data-name doesn't have
        // a name qualification syntax. Only identifiers do.
        if (CurrentEquals("IN OF"))
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 1200, """
                Name qualification prohibited.
                """)
            .WithSourceLine(dataToken, """
                Must not require qualification.
                """)
            .WithNote("""
                It must be a unique 77 or 01 level item.
                """)
            .CloseError();

            AnchorPoint(TokenContext.IsStatement, "RAISING");
            return;
        }

        // COBOL standard requirement, data-name used for
        // the returning item must not be the same of a parameter.
        if (headerDataNames.Contains(dataToken.Value))
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 1210, """
                Name conflict with a formal parameter.
                """)
            .WithSourceLine(dataToken,
                isParameter
                ? "Must not be the same as a parameter name."
                : "Must not be the same as another parameter name."
                )
            .CloseError();
            return;
        }

        var dataItem = References.FetchDataEntry(dataToken);

        // COBOL standard requirement, all parameters and returning items
        // must be defined in the linkage section only.
        if (dataItem.Section is not SourceScope.LinkageSection)
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 2050,
                isParameter
                ? "Invalid parameter definition."
                : "Invalid returning definition."
                )
            .WithSourceLine(dataToken, """
                Must be defined in the linkage section.
                """)
            .CloseError();
        }

        // COBOL standard requirement, must be 01 or 77.
        if (dataItem.LevelNumber is not (1 or 77))
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 2050,
                isParameter
                ? "Invalid parameter level number."
                : "Invalid returning level number."
                )
            .WithSourceLine(dataToken, """
                Must be a 77 or 01 level item.
                """)
            .CloseError();
        }

        // COBOL standard requirement, must not contain a BASED or REDEFINES clause.
        if (dataItem[DataClause.Based] || dataItem[DataClause.Redefines])
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 2050,
                isParameter
                ? "Invalid parameter clauses."
                : "Invalid returning clauses."
                )
            .WithSourceLine(dataToken, """
                Must not contain a BASED or REDEFINES clause.
                """)
            .CloseError();
        }

        // COBOL standard requirement, parameters passed by value must
        // have a class of numeric, message tag, object or pointer
        if (isParameter && !isByRef && !CheckByValueClass(dataItem.Class))
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 2050, """
                Invalid by value parameter class.
                """)
            .WithSourceLine(dataToken, """
                Must be of class Numeric, Message Tag, Object or Pointer.
                """)
            .CloseError();
        }

        headerDataNames.Add(dataToken.Value);

        var activeCallable = CompilerContext.ActiveCallable;

        if (isParameter)
        {
            var parameter = (dataItem, isOptional, isByRef);

            activeCallable.Parameters.Add(parameter);
            return;
        }

        activeCallable.Returning = dataItem;
    }

    private static bool CheckByValueClass(Classes _class)
    {
        return _class is Classes.Numeric or Classes.MessageTag or Classes.Object or Classes.Pointer;
    }

    private static void ProcedureBody()
    {
        var currentSource = CompilerContext.SourceTypes.Peek();

        bool isProcedureDeclarative = CurrentEquals("DECLARATIVES")
            || CurrentEquals(TokenType.Identifier) && PeekEquals(1, "SECTION");

        bool canContainStatements = currentSource switch
        {
            UnitKind.FunctionPrototype => false,
            UnitKind.ProgramPrototype => false,
            UnitKind.MethodPrototype => false,
            _ => true
        };

        if (canContainStatements && !isProcedureDeclarative) Statements.WithoutSections();

        if (canContainStatements && isProcedureDeclarative) Statements.WithSections();

        if (!canContainStatements && (CurrentEquals(TokenContext.IsStatement) || CurrentEquals(TokenType.Identifier)))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 205, """
                Misplaced statement definition.
                """)
            .WithSourceLine(Current(), """
                A statement cannot be defined here.
                """)
            .WithNote("""
                Prototypes must not contain any statements, sections or paragraphs.
                """)
            .CloseError();

            AnchorPoint("END IDENTIFICATION PROGRAM-ID FUNCTION-ID CLASS-ID INTERFACE-ID");
        }
    }

    public static void EndMarker()
    {
        var currentType = CompilerContext.SourceTypes.Peek();

        if (currentType != UnitKind.Program && !CurrentEquals("END") || currentType == UnitKind.Program && (!CurrentEquals(TokenType.EOF) && !CurrentEquals("END")))
        {
            string errorMessageChoice = currentType switch
            {
                UnitKind.Program or UnitKind.ProgramPrototype => """
                Missing END PROGRAM marker.
                """,

                UnitKind.Function or UnitKind.FunctionPrototype => """
                Missing END FUNCTION marker.
                """,

                UnitKind.Method or UnitKind.MethodPrototype or UnitKind.MethodGetter or UnitKind.MethodSetter => """
                Missing END METHOD marker.
                """,

                UnitKind.Class => """
                Missing END CLASS marker.
                """,

                UnitKind.Interface => """
                Missing END INTERFACE marker.
                """,

                UnitKind.Factory => """
                Missing END FACTORY marker.
                """,

                UnitKind.Object => """
                Missing END OBJECT marker.
                """,

                _ => throw new UnreachableException()
            };

            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 27, $"""
                End marker, {errorMessageChoice}
                """)
            .WithSourceLine(Peek(-1), """
                Expected a source unit end marker after this token
                """)
            .CloseError();

            return;
        }

        if (currentType == UnitKind.Program && CurrentEquals("EOF"))
        {
            CompilerContext.SourceTypes.Pop();
            return;
        }

        CompilerContext.SourceTypes.Pop();

        var endMarkerToken = CompilerContext.ActiveUnits.Pop();

        switch (currentType)
        {
            case UnitKind.Program:
            case UnitKind.ProgramPrototype:

                Expected("END");
                Expected("PROGRAM");

                EndMarkerErrorHandling(endMarkerToken);
                break;

            case UnitKind.Function:
            case UnitKind.FunctionPrototype:
                Expected("END");
                Expected("FUNCTION");

                EndMarkerErrorHandling(endMarkerToken);
                break;

            case UnitKind.Method:
            case UnitKind.MethodPrototype:
            case UnitKind.MethodGetter:
            case UnitKind.MethodSetter:
                Expected("END");
                Expected("METHOD");

                if (currentType is UnitKind.Method or UnitKind.MethodPrototype)
                {
                    References.Identifier(endMarkerToken);
                }

                if (!Expected(".", false))
                {
                    ErrorHandler
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                        End marker, missing separator period.
                        """)
                    .WithSourceLine(Peek(-1), """
                        Expected a separator period '. ' after this token
                        """)
                    .WithNote("""
                        Every end marker must end with a separator period
                        """)
                    .CloseError();

                    AnchorPoint("IDENTIFICATION METHOD-ID PROGRAM-ID FUNCTION-ID CLASS-ID INTERFACE-ID");
                }

                break;

            case UnitKind.Class:
                Expected("END");
                Expected("CLASS");

                EndMarkerErrorHandling(endMarkerToken);
                break;

            case UnitKind.Interface:
                Expected("END");
                Expected("INTERFACE");

                EndMarkerErrorHandling(endMarkerToken);
                break;

            case UnitKind.Factory:
                Expected("END");
                Expected("FACTORY");

                if (!Expected(".", false))
                {
                    ErrorHandler
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                        End marker, missing separator period.
                        """)
                    .WithSourceLine(Peek(-1), """
                        Expected a separator period '. ' after this token
                        """)
                    .WithNote("""
                        Every end marker must end with a separator period
                        """)
                    .CloseError();

                    AnchorPoint("OBJECT IDENTIFICATION PROGRAM-ID FUNCTION-ID CLASS-ID INTERFACE-ID");
                }

                break;

            case UnitKind.Object:
                Expected("END");
                Expected("OBJECT");

                if (!Expected(".", false))
                {
                    ErrorHandler
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 26, """
                        End marker, missing separator period.
                        """)
                    .WithSourceLine(Peek(-1), """
                        Expected a separator period '. ' after this token
                        """)
                    .WithNote("""
                        Every end marker must end with a separator period
                        """)
                    .CloseError();

                    AnchorPoint("OBJECT IDENTIFICATION PROGRAM-ID FUNCTION-ID CLASS-ID INTERFACE-ID");
                }

                break;
        }
    }

    private static void EndMarkerErrorHandling(Token token)
    {
        if (!References.Identifier(token, false))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 2, """
                Unexpected user-defined name.
                """)
            .WithSourceLine(Current(), $"""
                Expected the following identifier: {token.Value}.
                """)
            .WithSourceNote(token)
            .WithNote("""
                The end marker must match its source unit definition. 
                """)
            .CloseError();

            Continue();
        }

        if (!Expected(".", false))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                End marker, missing separator period.
                """)
            .WithSourceLine(Peek(-1), """
                Expected a separator period '. ' after this token
                """)
            .WithNote("""
                Every end marker must end with a separator period
                """)
            .CloseError();

            AnchorPoint("IDENTIFICATION OBJECT METHOD-ID PROGRAM-ID FUNCTION-ID CLASS-ID INTERFACE-ID");
        }
    }

    public static void ParseObjects()
    {
        if (CurrentEquals("FACTORY") || CurrentEquals("IDENTIFICATION") && PeekEquals(3, "FACTORY"))
        {
            IdentificationDivision.Parse();

            if (CurrentEquals("ENVIRONMENT"))
            {
                EnvironmentDivision.Parse();
            }

            if (CurrentEquals("DATA"))
            {
                DataDivision.Parse();
            }

            Expected("PROCEDURE");
            Expected("DIVISION");
            Expected(".");

            while (CurrentEquals("METHOD-ID") || CurrentEquals("IDENTIFICATION") && PeekEquals(3, "METHOD-ID"))
            {
                IdentificationDivision.Parse();

                if (CurrentEquals("ENVIRONMENT"))
                {
                    EnvironmentDivision.Parse();
                }

                if (CurrentEquals("DATA"))
                {
                    DataDivision.Parse();
                }

                ParseProcedural();

                EndMarker();
            }

            EndMarker();
        }

        if (CurrentEquals("OBJECT") || CurrentEquals("IDENTIFICATION") && PeekEquals(3, "OBJECT"))
        {
            IdentificationDivision.Parse();

            if (CurrentEquals("ENVIRONMENT"))
            {
                EnvironmentDivision.Parse();
            }

            if (CurrentEquals("DATA"))
            {
                DataDivision.Parse();
            }

            Expected("PROCEDURE");
            Expected("DIVISION");
            Expected(".");

            while (CurrentEquals("METHOD-ID") || (CurrentEquals("IDENTIFICATION") && PeekEquals(3, "METHOD-ID")))
            {
                IdentificationDivision.Parse();

                if (CurrentEquals("ENVIRONMENT"))
                {
                    EnvironmentDivision.Parse();
                }

                if (CurrentEquals("DATA"))
                {
                    DataDivision.Parse();
                }

                ParseProcedural();

                EndMarker();
            }

            EndMarker();
        }
    }

    public static void ParseInterface()
    {
        if (CurrentEquals("PROCEDURE"))
        {
            Expected("PROCEDURE");
            Expected("DIVISION");

            if (!Expected(".", false))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                    Division header, missing separator period.
                    """)
                .WithSourceLine(Peek(-1), """
                    Expected a separator period '. ' after this token
                    """)
                .WithNote("""
                    Every division header must end with a separator period
                    """)
                .CloseError();

                AnchorPoint(TokenContext.IsStatement);
            }

            while (CurrentEquals("METHOD-ID") || CurrentEquals("IDENTIFICATION") && PeekEquals(3, "METHOD-ID"))
            {
                IdentificationDivision.Parse();

                if (CurrentEquals("ENVIRONMENT"))
                {
                    EnvironmentDivision.Parse();
                }

                if (CurrentEquals("DATA"))
                {
                    DataDivision.Parse();
                }

                if (CurrentEquals("PROCEDURE"))
                {
                    ParseProcedural();
                }

                EndMarker();
            }
        }
    }
}
