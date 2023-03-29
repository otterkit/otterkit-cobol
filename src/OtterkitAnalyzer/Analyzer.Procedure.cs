using System.Diagnostics;

namespace Otterkit;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static partial class Analyzer
{
    // Method responsible for parsing the PROCEDURE DIVISION.
    // That includes the user-defined paragraphs, sections and declaratives
    // or when parsing OOP COBOL code, it's responsible for parsing COBOL methods, objects and factories. 
    // It is also responsible for showing appropriate error messages when an error occurs in the PROCEDURE DIVISION.
    public static void PROCEDURE()
    {
        Expected("PROCEDURE");
        Expected("DIVISION");

        CurrentSection = CurrentScope.ProcedureDivision;
        var currentSource = SourceType.Peek();

        if (CurrentEquals("USING"))
        {
            Expected("USING");

            while (CurrentEquals("BY", "REFERENCE", "VALUE"))
            {
                // TODO: This is incorrect:
                // By reference can be implicit

                if (CurrentEquals("BY") && !LookaheadEquals(1, "VALUE", "REFERENCE"))
                {
                    ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                    The USING BY clause in the procedure division header must be followed by "VALUE" or "REFERENCE"
                    """);
                    ErrorHandler.Analyzer.PrettyError(FileName, Current());

                    CombinedAnchorPoint(TokenContext.IsStatement, "RETURNING", ".");
                }

                if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && LookaheadEquals(1, "REFERENCE"))
                {
                    Optional("BY");
                    Expected("REFERENCE");

                    if (CurrentEquals("OPTIONAL"))
                    {
                        Expected("OPTIONAL");
                    }

                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY REFERENCE clause must contain at least one data item name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }
                    
                    // TODO: Reimplement parameter item resolution

                    Identifier();
                    while (CurrentEquals(TokenType.Identifier) || CurrentEquals("OPTIONAL"))
                    {
                        if (CurrentEquals("OPTIONAL"))
                        {
                            Expected("OPTIONAL");
                        }
                        // TODO: Reimplement parameter item resolution
                        Identifier();
                    }
                }

                if (CurrentEquals("VALUE") || CurrentEquals("BY") && LookaheadEquals(1, "VALUE"))
                {
                    Optional("BY");
                    Expected("VALUE");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY VALUE clause must contain at least one data item name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }
                    
                    // TODO: Reimplement parameter item resolution
                    Identifier();
                    while (CurrentEquals(TokenType.Identifier))
                    {
                        // TODO: Reimplement parameter item resolution
                        Identifier();
                    }
                }
            }
        }

        if (SourceType.Peek() is SourceUnit.Function or SourceUnit.FunctionPrototype)
        {
            Expected("RETURNING");
            ReturningDataName();
        }
        else if (CurrentEquals("RETURNING"))
        {
            Expected("RETURNING");
            ReturningDataName();
        }

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Division header, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
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
    public static void ReturningDataName()
    {
        if (!CurrentEquals(TokenType.Identifier))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 75, """
                Missing returning data item.
                """)
            .WithSourceLine(Lookahead(-1), $"""
                Missing returning identifier after this token.
                """)
            .CloseError();
            return;
        }

        var sourceUnit = CurrentSourceUnit;

        var (exists, isUnique) = sourceUnit.Definitions.LocalExistsAndIsUnique(Current().Value);

        if (!exists)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                Reference to undefined identifier.
                """)
            .WithSourceLine(Current(), $"""
                Identifier name does not exist in the current context.
                """)
            .CloseError();

            Continue();
            return;
        }

        if (exists && !isUnique)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                Reference to non-unique identifier.
                """)
            .WithSourceLine(Current(), $"""
                Identifier name requires a qualifier.
                """)
            .CloseError();

            Continue();
            return;
        }

        var returning = sourceUnit.Definitions.GetUniqueLocalByName(Current().Value);

        sourceUnit.Returning = returning;

        // TODO: Handle name qualifiers.

        Identifier();
    }

    public static void ProcedureBody()
    {
        var currentSource = SourceType.Peek();

        bool isProcedureDeclarative = CurrentEquals("DECLARATIVES")
            || CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION");

        bool canContainStatements = currentSource switch
        {
            SourceUnit.FunctionPrototype => false,
            SourceUnit.ProgramPrototype => false,
            SourceUnit.MethodPrototype => false,
            _ => true
        };

        if (canContainStatements && !isProcedureDeclarative) ParseStatements();

        if (canContainStatements && isProcedureDeclarative) DeclarativeProcedure();

        if (!canContainStatements && (CurrentEquals(TokenContext.IsStatement) || CurrentEquals(TokenType.Identifier)))
        {
            Error
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

            AnchorPoint("END", "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
        }
    }

    public static void EndMarker()
    {
        SourceUnit currentSource = SourceType.Peek();

        if (currentSource != SourceUnit.Program && !CurrentEquals("END") || currentSource == SourceUnit.Program && (!CurrentEquals(TokenType.EOF) && !CurrentEquals("END")))
        {
            string errorMessageChoice = currentSource switch
            {
                SourceUnit.Program or SourceUnit.ProgramPrototype => """
                Missing END PROGRAM marker.
                """,

                SourceUnit.Function or SourceUnit.FunctionPrototype => """
                Missing END FUNCTION marker.
                """,

                SourceUnit.Method or SourceUnit.MethodPrototype or SourceUnit.MethodGetter or SourceUnit.MethodSetter => """
                Missing END METHOD marker.
                """,

                SourceUnit.Class => """
                Missing END CLASS marker.
                """,

                SourceUnit.Interface => """
                Missing END INTERFACE marker.
                """,

                SourceUnit.Factory => """
                Missing END FACTORY marker.
                """,

                SourceUnit.Object => """
                Missing END OBJECT marker.
                """,

                _ => throw new UnreachableException()
            };

            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 27, $"""
                End marker, {errorMessageChoice}
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a source unit end marker after this token
                """)
            .CloseError();

            return;
        }

        if (currentSource == SourceUnit.Program && CurrentEquals("EOF"))
        {
            SourceType.Pop();
            return;
        }

        switch (currentSource)
        {
            
            case SourceUnit.Program:
            case SourceUnit.ProgramPrototype:
                SourceType.Pop();

                Expected("END");
                Expected("PROGRAM");

                EndMarkerErrorHandling(CurrentId.Pop());

                break;

            case SourceUnit.Function:
            case SourceUnit.FunctionPrototype:
                SourceType.Pop();

                Expected("END");
                Expected("FUNCTION");

                EndMarkerErrorHandling(CurrentId.Pop());

                break;

            case SourceUnit.Method:
            case SourceUnit.MethodPrototype:
            case SourceUnit.MethodGetter:
            case SourceUnit.MethodSetter:
                SourceType.Pop();

                Expected("END");
                Expected("METHOD");
                if (currentSource is SourceUnit.Method or SourceUnit.MethodPrototype)
                    Identifier(CurrentId.Pop());

                if (currentSource is SourceUnit.MethodGetter or SourceUnit.MethodSetter)
                    CurrentId.Pop();

                if (!Expected(".", false))
                {
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                        End marker, missing separator period.
                        """)
                    .WithSourceLine(Lookahead(-1), """
                        Expected a separator period '. ' after this token
                        """)
                    .WithNote("""
                        Every end marker must end with a separator period
                        """)
                    .CloseError();

                    AnchorPoint("IDENTIFICATION", "METHOD-ID", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                }

                break;

            case SourceUnit.Class:
                SourceType.Pop();

                Expected("END");
                Expected("CLASS");

                EndMarkerErrorHandling(CurrentId.Pop());

                break;

            case SourceUnit.Interface:
                SourceType.Pop();

                Expected("END");
                Expected("INTERFACE");

                EndMarkerErrorHandling(CurrentId.Pop());

                break;

            case SourceUnit.Factory:
                SourceType.Pop();

                Expected("END");
                Expected("FACTORY");
                
                if (!Expected(".", false))
                {
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                        End marker, missing separator period.
                        """)
                    .WithSourceLine(Lookahead(-1), """
                        Expected a separator period '. ' after this token
                        """)
                    .WithNote("""
                        Every end marker must end with a separator period
                        """)
                    .CloseError();

                    AnchorPoint("OBJECT", "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                }
                
                break;

            case SourceUnit.Object:
                SourceType.Pop();

                Expected("END");
                Expected("OBJECT");
                
                if (!Expected(".", false))
                {
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 26,"""
                        End marker, missing separator period.
                        """)
                    .WithSourceLine(Lookahead(-1), """
                        Expected a separator period '. ' after this token
                        """)
                    .WithNote("""
                        Every end marker must end with a separator period
                        """)
                    .CloseError();

                    AnchorPoint("OBJECT", "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                }
                
                break;
        }
    }

    public static void EndMarkerErrorHandling(Token token)
    {
        if (!Identifier(token, false))
        {
            Error
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
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                End marker, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token
                """)
            .WithNote("""
                Every end marker must end with a separator period
                """)
            .CloseError();

            AnchorPoint("IDENTIFICATION", "OBJECT", "METHOD-ID", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
        }
    }

    public static void ClassObjects()
    {
        if (CurrentEquals("FACTORY") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "FACTORY"))
        {
            IDENTIFICATION();

            if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

            if (CurrentEquals("DATA")) DATA();
            Expected("PROCEDURE");
            Expected("DIVISION");
            Expected(".");

            while (CurrentEquals("METHOD-ID") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "METHOD-ID"))
            {
                IDENTIFICATION();
                if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

                if (CurrentEquals("DATA")) DATA();

                PROCEDURE();

                EndMarker();
            }

            EndMarker();
        }

        if (CurrentEquals("OBJECT") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "OBJECT"))
        {
            IDENTIFICATION();

            if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

            if (CurrentEquals("DATA")) DATA();

            Expected("PROCEDURE");
            Expected("DIVISION");
            Expected(".");

            while (CurrentEquals("METHOD-ID") || (CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "METHOD-ID")))
            {
                IDENTIFICATION();
                if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

                if (CurrentEquals("DATA")) DATA();

                PROCEDURE();

                EndMarker();
            }

            EndMarker();
        }
    }

    public static void InterfaceProcedure()
    {
        if (CurrentEquals("PROCEDURE"))
        {
            Expected("PROCEDURE");
            Expected("DIVISION");
            if (!Expected(".", false))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                    Division header, missing separator period.
                    """)
                .WithSourceLine(Lookahead(-1), """
                    Expected a separator period '. ' after this token
                    """)
                .WithNote("""
                    Every division header must end with a separator period
                    """)
                .CloseError();

                AnchorPoint(TokenContext.IsStatement);
            }

            while (CurrentEquals("METHOD-ID") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "METHOD-ID"))
            {
                IDENTIFICATION();

                if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

                if (CurrentEquals("DATA")) DATA();

                if (CurrentEquals("PROCEDURE")) 
                {
                    PROCEDURE();
                }

                EndMarker();
            }
        }
    }
}
