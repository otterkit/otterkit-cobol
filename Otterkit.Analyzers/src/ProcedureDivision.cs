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

        CompilerContext.ActiveScope = CurrentScope.ProcedureDivision;

        var currentSource = CompilerContext.SourceTypes.Peek();

        if (CurrentEquals("USING"))
        {
            Expected("USING");
            Using();
        }

        if (CompilerContext.SourceTypes.Peek() is SourceUnit.Function or SourceUnit.FunctionPrototype)
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
    private static void Using()
    {
        while (CurrentEquals("BY", "REFERENCE", "VALUE") || CurrentEquals(TokenType.Identifier))
        {   
            if (CurrentEquals(TokenType.Identifier))
            {
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

            if (CurrentEquals("BY") && !LookaheadEquals(1, "VALUE", "REFERENCE"))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 128,"""
                    Using phrase, missing keyword.
                    """)
                .WithSourceLine(Current(), """
                    Expected 'VALUE' or 'REFERENCE' after this token
                    """)
                .CloseError();

                AnchorPoint(TokenContext.IsStatement, "RETURNING", ".");
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
                    ErrorHandler
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 128,"""
                        Using phrase, missing identifier.
                        """)
                    .WithSourceLine(Current(), """
                        BY REFERENCE phrase must contain at least one data item name.
                        """)
                    .CloseError();
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
                    ErrorHandler
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 128,"""
                        Using phrase, missing identifier.
                        """)
                    .WithSourceLine(Current(), """
                        BY VALUE phrase must contain at least one data item name.
                        """)
                    .CloseError();
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

    private static void Returning()
    {
        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 75, """
                Missing returning data item.
                """)
            .WithSourceLine(Lookahead(-1), $"""
                Missing returning identifier after this token.
                """)
            .CloseError();
            return;
        }

        Identifier();
    }

    private static void ProcedureBody()
    {
        var currentSource = CompilerContext.SourceTypes.Peek();

        bool isProcedureDeclarative = CurrentEquals("DECLARATIVES")
            || CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION");

        bool canContainStatements = currentSource switch
        {
            SourceUnit.FunctionPrototype => false,
            SourceUnit.ProgramPrototype => false,
            SourceUnit.MethodPrototype => false,
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

            AnchorPoint("END", "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
        }
    }

    public static void EndMarker()
    {
        var currentType = CompilerContext.SourceTypes.Peek();

        if (currentType != SourceUnit.Program && !CurrentEquals("END") || currentType == SourceUnit.Program && (!CurrentEquals(TokenType.EOF) && !CurrentEquals("END")))
        {
            string errorMessageChoice = currentType switch
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

            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 27, $"""
                End marker, {errorMessageChoice}
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a source unit end marker after this token
                """)
            .CloseError();

            return;
        }

        if (currentType == SourceUnit.Program && CurrentEquals("EOF"))
        {
            CompilerContext.SourceTypes.Pop();
            return;
        }

        CompilerContext.SourceTypes.Pop();

        var endMarkerToken = CompilerContext.ActiveUnits.Pop();

        switch (currentType)
        {
            case SourceUnit.Program:
            case SourceUnit.ProgramPrototype:

                Expected("END");
                Expected("PROGRAM");

                EndMarkerErrorHandling(endMarkerToken);
                break;

            case SourceUnit.Function:
            case SourceUnit.FunctionPrototype:
                Expected("END");
                Expected("FUNCTION");

                EndMarkerErrorHandling(endMarkerToken);
                break;

            case SourceUnit.Method:
            case SourceUnit.MethodPrototype:
            case SourceUnit.MethodGetter:
            case SourceUnit.MethodSetter:
                Expected("END");
                Expected("METHOD");

                if (currentType is SourceUnit.Method or SourceUnit.MethodPrototype)
                {
                    Identifier(endMarkerToken);
                }

                if (!Expected(".", false))
                {
                    ErrorHandler
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
                Expected("END");
                Expected("CLASS");

                EndMarkerErrorHandling(endMarkerToken);
                break;

            case SourceUnit.Interface:
                Expected("END");
                Expected("INTERFACE");

                EndMarkerErrorHandling(endMarkerToken);
                break;

            case SourceUnit.Factory:
                Expected("END");
                Expected("FACTORY");
                
                if (!Expected(".", false))
                {
                    ErrorHandler
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
                Expected("END");
                Expected("OBJECT");
                
                if (!Expected(".", false))
                {
                    ErrorHandler
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

    private static void EndMarkerErrorHandling(Token token)
    {
        if (!Identifier(token, false))
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

    public static void ParseObjects()
    {
        if (CurrentEquals("FACTORY") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "FACTORY"))
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

            while (CurrentEquals("METHOD-ID") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "METHOD-ID"))
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

        if (CurrentEquals("OBJECT") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "OBJECT"))
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

            while (CurrentEquals("METHOD-ID") || (CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "METHOD-ID")))
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
