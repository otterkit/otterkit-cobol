using System.Diagnostics.CodeAnalysis;
using System.Diagnostics;
using System.Text;

namespace Otterkit;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static partial class Analyzer
{
    private static string FileName = string.Empty;
    private static CurrentScope CurrentSection;
    private static readonly Stack<Token> CurrentId = new();
    private static readonly Stack<SourceUnit> SourceType = new();

    /// <summary>
    /// Otterkit COBOL Syntax Analyzer
    /// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
    /// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
    /// </summary>
    public static List<Token> Analyze(List<Token> tokenList, string entryPoint)
    {
        FileName = entryPoint;

        // Call the parser's main recursive method
        // This should only return when the parser reaches the true EOF token
        Source();

        // If a parsing error has occured, terminate the compilation process.
        // We do not want the compiler to continue when the source code is not valid.
        if (ErrorHandler.HasError) ErrorHandler.Terminate("parsing");

        // Return parsed list of tokens.
        return CompilerContext.SourceTokens;
    }

    // Source() is the main method of the parser.
    // It's responsible for parsing COBOL divisions until the EOF token.
    // If EOF was not returned as the last Token in the list then,
    // the parser has not finished reading through the list of tokens correctly.
    public static void Source()
    {
        IDENTIFICATION();

        if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

        if (CurrentEquals("DATA")) DATA();

        bool notClassOrInterface = SourceType.Peek() switch
        {
            SourceUnit.Class => false,
            SourceUnit.Interface => false,
            _ => true
        };

        if (notClassOrInterface)
        {
            if (CurrentEquals("PROCEDURE")) 
                PROCEDURE(CompilerContext.CurrentCallable[0]);
        }
        else if (SourceType.Peek() == SourceUnit.Class)
        {
            ClassObjects();
        }
        else if (SourceType.Peek() == SourceUnit.Interface)
        {
            InterfaceProcedure();
        }

        EndMarker();

        if (CurrentEquals("IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID"))
        {
            Source();
        }

        if (CurrentEquals("EOF") && CurrentIndex() < CompilerContext.SourceTokens.Count - 1)
        {
            FileName = Lookahead(1).FetchFile;

            Continue();
            Source();
        }
    }


    // Method responsible for parsing the IDENTIFICATION DIVISION.
    // That includes PROGRAM-ID, FUNCTION-ID, CLASS-ID, METHOD-ID, INTERFACE-ID, OBJECT, FACTORY and OPTIONS paragraphs.
    // It is also responsible for showing appropriate error messages when an error occurs in the IDENTIFICATION DIVISION.
    public static void IDENTIFICATION()
    {
        if (CurrentEquals("IDENTIFICATION"))
        {
            Expected("IDENTIFICATION");
            Expected("DIVISION");

            Expected(".", """
            Missing separator period at the end of this IDENTIFICATION DIVISION header, every division header must end with a separator period
            """, -1, "PROGRAM-ID", "FUNCTION-ID", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        if (!CurrentEquals("PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "METHOD-ID", "INTERFACE-ID", "OBJECT", "FACTORY"))
        {
            Expected("PROGRAM-ID", """
            Missing source unit ID name (PROGRAM-ID, FUNCTION-ID, CLASS-ID...), the identification division header is optional but every source unit must still have an ID.
            """, 0, "OPTIONS", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        if (CurrentEquals("PROGRAM-ID"))
        {
            ProgramId();
        }

        else if (CurrentEquals("FUNCTION-ID"))
        {
            FunctionId();
        }

        else if (CurrentEquals("CLASS-ID"))
        {
            ClassId();
        }

        else if (CurrentEquals("INTERFACE-ID"))
        {
            InterfaceId();
        }

        else if (SourceType.Peek() is SourceUnit.Class && CurrentEquals("FACTORY"))
        {
            Factory();
        }

        else if (SourceType.Peek() is SourceUnit.Class && CurrentEquals("OBJECT"))
        {
            Object();
        }

        else if (SourceType.Peek() is SourceUnit.Object or SourceUnit.Factory or SourceUnit.Interface && CurrentEquals("METHOD-ID"))
        {
            MethodId();
        }

        if (CurrentEquals("OPTIONS"))
        {
            Options();
        }
    }

    public static void Options()
    {
        bool shouldHavePeriod = false;

        Expected("OPTIONS");
        Expected(".");

        if (CurrentEquals("ARITHMETIC"))
        {
            Expected("ARITHMETIC");
            Optional("IS");
            Choice("NATIVE", "STANDARD-BINARY", "STANDARD-DECIMAL");

            shouldHavePeriod = true;
        }

        if (CurrentEquals("DEFAULT"))
        {
            Expected("DEFAULT");
            Expected("ROUNDED");
            Optional("MODE");
            Optional("IS");
            Choice(
                "AWAY-FROM-ZERO", "NEAREST-AWAY-FROM-ZERO",
                "NEAREST-EVEN", "NEAREST-TOWARD-ZERO",
                "PROHIBITED", "TOWARD-GREATER",
                "TOWARD-LESSER", "TRUNCATION"
            );

            shouldHavePeriod = true;
        }

        if (CurrentEquals("ENTRY-CONVENTION"))
        {
            Expected("ENTRY-CONVENTION");
            Optional("IS");
            Expected("COBOL");

            shouldHavePeriod = true;
        }

        if (shouldHavePeriod) Expected(".");
    }


    // The following methods are responsible for parsing the -ID paragraph.
    // That includes the program, user-defined function, method, class, interface, factory or object identifier that should be specified right after.
    // This is where SourceId and SourceType get their values for a COBOL source unit.
    public static void ProgramId()
    {
        Expected("PROGRAM-ID");
        Expected(".");

        CurrentId.Push(Current());
        SourceType.Push(SourceUnit.Program);
        CurrentSection = CurrentScope.ProgramId;

        Identifier();
        if (CurrentEquals("AS"))
        {
            Expected("AS");
            CurrentId.Pop();
            String();
            CurrentId.Push(Lookahead(-1));
        }

        if (CurrentEquals("IS", "COMMON", "INITIAL", "RECURSIVE", "PROTOTYPE"))
        {
            bool isCommon = false;
            bool isInitial = false;
            bool isPrototype = false;
            bool isRecursive = false;

            Optional("IS");

            while (CurrentEquals("COMMON", "INITIAL", "RECURSIVE", "PROTOTYPE"))
            {
                if (CurrentEquals("COMMON"))
                {
                    Expected("COMMON");
                    isCommon = true;
                }

                if (CurrentEquals("INITIAL"))
                {
                    Expected("INITIAL");
                    isInitial = true;
                }

                if (CurrentEquals("RECURSIVE"))
                {
                    Expected("RECURSIVE");
                    isRecursive = true;
                }

                if (CurrentEquals("PROTOTYPE"))
                {
                    Expected("PROTOTYPE");
                    SourceType.Pop();
                    SourceType.Push(SourceUnit.ProgramPrototype);
                    isPrototype = true;
                }
            }

            if (isPrototype && (isCommon || isInitial || isRecursive))
            {
                Error
                .Build(ErrorType.Syntax, ConsoleColor.Red, 55,"""
                    Invalid program prototype definition.
                    """)
                .WithSourceLine(CurrentId.Peek(), """
                    Program prototypes cannot be defined as common, initial or recursive.
                    """)
                .CloseError();
            }

            if (isInitial && isRecursive)
            {
                Error
                .Build(ErrorType.Syntax, ConsoleColor.Red, 55,"""
                    Invalid program definition.
                    """)
                .WithSourceLine(CurrentId.Peek(), """
                    Initial programs cannot be defined as recursive.
                    """)
                .CloseError();
            }

            if (!isPrototype) Optional("PROGRAM");
        }

        var globals = SymbolTable.SourceUnitGlobals;

        var signature = new CallableSignature(CurrentId.Peek(), SourceType.Peek());

        globals.TryAddGlobalReference(CurrentId.Peek().Value, signature);

        CompilerContext.CurrentCallable[0] = signature;

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Program definition, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }
    }

    public static void FunctionId()
    {
        Expected("FUNCTION-ID");
        Expected(".");

        SourceType.Push(SourceUnit.Function);
        CurrentSection = CurrentScope.FunctionId;

        Identifier();

        if (CurrentEquals("AS"))
        {
            Expected("AS");
            String();
        }

        if (CurrentEquals("IS", "PROTOTYPE"))
        {
            Optional("IS");
            Expected("PROTOTYPE");
            SourceType.Pop();
            SourceType.Push(SourceUnit.FunctionPrototype);
        }

        var globals = SymbolTable.SourceUnitGlobals;

        var signature = new CallableSignature(CurrentId.Peek(), SourceType.Peek());

        globals.TryAddGlobalReference(CurrentId.Peek().Value, signature);

        CompilerContext.CurrentCallable[0] = signature;

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Function definition, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }
    }

    public static void ClassId()
    {
        Expected("CLASS-ID");
        Expected(".");

        CurrentId.Push(Current());

        SourceType.Push(SourceUnit.Class);
        CurrentSection = CurrentScope.ClassId;

        Identifier();

        if (CurrentEquals("AS"))
        {
            Expected("AS");
            String();
        }

        if (CurrentEquals("IS", "FINAL"))
        {
            Optional("IS");
            Expected("FINAL");
        }

        if (CurrentEquals("INHERITS"))
        {
            Expected("INHERITS");
            Optional("FROM");

            if (!CurrentEquals(TokenType.Identifier))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 60,"""
                    Class definition, missing INHERITS class.
                    """)
                .WithSourceLine(Lookahead(-1), """
                    The INHERITS phrase must contain a class name.
                    """)
                .CloseError();
            }

            Identifier();
        }

        if (CurrentEquals("USING"))
        {
            Expected("USING");
            if (!CurrentEquals(TokenType.Identifier))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 60,"""
                    Class definition, missing using parameter.
                    """)
                .WithSourceLine(Lookahead(-1), """
                    The USING phrase must contain at least one parameter.
                    """)
                .CloseError();
            }

            Identifier();
            while (CurrentEquals(TokenType.Identifier)) Identifier();
        }

        var signature = new ClassSignature(CurrentId.Peek(), SourceType.Peek());

        SymbolTable.SourceUnitGlobals
            .TryAddGlobalReference(CurrentId.Peek().Value, signature);

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Class definition, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION", "ENVIRONMENT", "DATA", "FACTORY", "OBJECT");
        }
    }

    public static void InterfaceId()
    {
        Expected("INTERFACE-ID");
        Expected(".");
        
        CurrentId.Push(Current());

        SourceType.Push(SourceUnit.Interface);
        CurrentSection = CurrentScope.InterfaceId;

        Identifier();

        if (CurrentEquals("AS"))
        {
            Expected("AS");
            String();
        }

        if (CurrentEquals("INHERITS"))
        {
            Expected("INHERITS");
            Optional("FROM");

            if (!CurrentEquals(TokenType.Identifier))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 60,"""
                    Interface definition, missing INHERITS class.
                    """)
                .WithSourceLine(Lookahead(-1), """
                    The INHERITS phrase must contain at least one interface name.
                    """)
                .CloseError();
            }

            Identifier();
            while (CurrentEquals(TokenType.Identifier)) Identifier();
        }

        if (CurrentEquals("USING"))
        {
            Expected("USING");
            if (!CurrentEquals(TokenType.Identifier))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 60,"""
                    Interface definition, missing using parameter.
                    """)
                .WithSourceLine(Lookahead(-1), """
                    The USING phrase must contain at least one parameter.
                    """)
                .CloseError();
            }

            Identifier();
            while (CurrentEquals(TokenType.Identifier)) Identifier();
        }

        var signature = new InterfaceSignature(CurrentId.Peek(), SourceType.Peek());

        SymbolTable.SourceUnitGlobals
            .TryAddGlobalReference(CurrentId.Peek().Value, signature);

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Interface definition, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }
    }

    public static void MethodId()
    {
        Expected("METHOD-ID");
        Expected(".");

        CurrentSection = CurrentScope.MethodId;
        var currentSource = SourceType.Peek();
        var currentId = CurrentId.Peek();

        if (currentSource != SourceUnit.Interface && CurrentEquals("GET"))
        {
            Expected("GET");
            Expected("PROPERTY");
            CurrentId.Push(Current());
            SourceType.Push(SourceUnit.MethodGetter);

            Identifier();

        }
        else if (currentSource != SourceUnit.Interface && CurrentEquals("SET"))
        {
            Expected("SET");
            Expected("PROPERTY");
            CurrentId.Push(Current());
            SourceType.Push(SourceUnit.MethodSetter);

            Identifier();
        }
        else // If not a getter or a setter
        {

            CurrentId.Push(Current());
            Identifier();

            if (CurrentEquals("AS"))
            {
                Expected("AS");
                String();
            }

            if (currentSource == SourceUnit.Interface)
            {
                SourceType.Push(SourceUnit.MethodPrototype);
            }
            else
            {
                SourceType.Push(SourceUnit.Method);
            }
        }

        if (CurrentEquals("OVERRIDE")) Expected("OVERRIDE");

        if (CurrentEquals("IS", "FINAL"))
        {
            Optional("IS");
            Expected("FINAL");
        }

        if (!Expected(".", false))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25,"""
                Interface definition, missing separator period.
                """)
            .WithSourceLine(Lookahead(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        if (currentSource is SourceUnit.Interface)
        {
            var parentInterface = SymbolTable.GetSignature<InterfaceSignature>(currentId.Value);

            var methodPrototype = new CallableSignature(CurrentId.Peek(), SourceType.Peek());

            parentInterface.Methods.Add(methodPrototype);

            CompilerContext.CurrentCallable[0] = methodPrototype;
        }

        var parentClass = SymbolTable.GetSignature<ClassSignature>(currentId.Value);

        var method = new CallableSignature(CurrentId.Peek(), SourceType.Peek());

        if (currentSource is SourceUnit.Object)
        {
            parentClass.ObjectMethods.Add(method);
        }

        if (currentSource is SourceUnit.Factory)
        {
            parentClass.FactoryMethods.Add(method);
        }
        
        CompilerContext.CurrentCallable[0] = method;
    }

    public static void Factory()
    {
        Expected("FACTORY");
        Expected(".");

        SourceType.Push(SourceUnit.Factory);

        if (CurrentEquals("IMPLEMENTS"))
        {
            Expected("IMPLEMENTS");
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                The IMPLEMENTS clause must contain at least one interface name.
                """);
                ErrorHandler.Analyzer.PrettyError(FileName, Current());
            }

            Identifier();
            while (CurrentEquals(TokenType.Identifier)) Identifier();

            Expected(".");
        }
    }

    public static void Object()
    {
        Expected("OBJECT");
        Expected(".");

        SourceType.Push(SourceUnit.Object);

        if (CurrentEquals("IMPLEMENTS"))
        {
            Expected("IMPLEMENTS");
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                The IMPLEMENTS clause must contain at least one interface name.
                """);
                ErrorHandler.Analyzer.PrettyError(FileName, Current());
            }

            Identifier();
            while (CurrentEquals(TokenType.Identifier)) Identifier();

            Expected(".");
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

                PROCEDURE(CompilerContext.CurrentCallable[0]);

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

                PROCEDURE(CompilerContext.CurrentCallable[0]);

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
                    PROCEDURE(CompilerContext.CurrentCallable[0]);
                }

                EndMarker();
            }
        }
    }

    // Method responsible for parsing the ENVIRONMENT DIVISION.
    // That includes the CONFIGURATION and the INPUT-OUTPUT sections.
    // It is also responsible for showing appropriate error messages when an error occurs in the ENVIRONMENT DIVISION.
    public static void ENVIRONMENT()
    {
        Expected("ENVIRONMENT");
        Expected("DIVISION");
        CurrentSection = CurrentScope.EnvironmentDivision;

        Expected(".", """
        Missing separator period at the end of this ENVIRONMENT DIVISION header, every division header must end with a separator period
        """, -1, "DATA", "PROCEDURE", "PROGRAM-ID", "FUNCTION-ID");

        if (CurrentEquals("CONFIGURATION"))
        {
            Expected("CONFIGURATION");
            Expected("SECTION");
            Expected(".", """
            Missing separator period at the end of this CONFIGURATION SECTION header, every section must end with a separator period
            """, -1, "REPOSITORY", "DATA", "PROCEDURE", "PROGRAM-ID", "FUNCTION-ID");

            if (CurrentEquals("REPOSITORY")) REPOSITORY();
        }
    }

    public static void REPOSITORY()
    {
        Expected("REPOSITORY");
        CurrentSection = CurrentScope.Repository;

        Expected(".", """
        Missing separator period at the end of this REPOSITORY paragraph header, every paragraph must end with a separator period
        """, -1, "CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");

        while (CurrentEquals("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY"))
        {
            if (CurrentEquals("CLASS"))
            {
                Expected("CLASS");
                Identifier();

                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }

                if (CurrentEquals("EXPANDS"))
                {
                    Expected("EXPANDS");
                    Identifier();
                    Expected("USING");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING clause must contain at least one class, object or interface name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (!CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, TokenType.Identifier))
                    {
                        AnchorPoint("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");
                    }

                    Identifier();
                    while (CurrentEquals(TokenType.Identifier)) Identifier();
                }
            }

            if (CurrentEquals("INTERFACE"))
            {
                Expected("INTERFACE");
                Identifier();

                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }

                if (CurrentEquals("EXPANDS"))
                {
                    Expected("EXPANDS");
                    Identifier();
                    Expected("USING");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING clause must contain at least one class, object or interface name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (!CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, TokenType.Identifier))
                    {
                        AnchorPoint("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");
                    }

                    Identifier();
                    while (CurrentEquals(TokenType.Identifier)) Identifier();
                }
            }

            if (CurrentEquals("FUNCTION"))
            {
                Expected("FUNCTION");
                if (CurrentEquals("ALL"))
                {
                    Expected("ALL");
                    Expected("INTRINSIC");
                }
                else if (CurrentEquals(TokenType.IntrinsicFunction))
                {
                    Continue();
                    while (CurrentEquals(TokenType.IntrinsicFunction) || CurrentEquals("RANDOM"))
                    {
                        Continue();
                    }

                    Expected("INTRINSIC");

                    if (!CurrentEquals("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "."))
                    {
                        AnchorPoint("CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");
                    }
                }
                else
                {
                    Identifier();
                    if (CurrentEquals("AS"))
                    {
                        Expected("AS");
                        String();
                    }
                }
            }

            if (CurrentEquals("PROGRAM"))
            {
                Expected("PROGRAM");
                Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }
            }

            if (CurrentEquals("PROPERTY"))
            {
                Expected("PROPERTY");
                Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }
            }
        }

        Expected(".", """
        Missing separator period at the end of this REPOSITORY paragraph body, the last definition in the REPOSITORY paragraph must end with a period
        """, -1, "CLASS", "INTERFACE", "FUNCTION", "PROGRAM", "PROPERTY", "DATA", "PROCEDURE");
    }

    // Method responsible for parsing the PROCEDURE DIVISION.
    // That includes the user-defined paragraphs, sections and declaratives
    // or when parsing OOP COBOL code, it's responsible for parsing COBOL methods, objects and factories. 
    // It is also responsible for showing appropriate error messages when an error occurs in the PROCEDURE DIVISION.
    public static void PROCEDURE(CallableSignature currentCallable)
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
            ReturningDataName(currentCallable);
        }
        else if (CurrentEquals("RETURNING"))
        {
            Expected("RETURNING");
            ReturningDataName(currentCallable);
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
    public static void ReturningDataName(CallableSignature currentCallable)
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

        var (exists, isUnique) = SymbolTable.VariableExistsAndIsUnique(Current().Value);

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
            return;
        }

        var returning = SymbolTable.GetUniqueVariableByName(Current().Value);

        currentCallable.Returning = returning;

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
            ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
            The procedure division of function, program and method prototypes must not contain any statements, sections or paragraphs
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, Current());

            AnchorPoint("END");
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

    public static (Token, SourceUnit) GetRootId()
    {
        var rootId = CurrentId.PeekBehind();
        var rootType = SourceType.PeekBehind();

        return (rootId, rootType);
    }
}
