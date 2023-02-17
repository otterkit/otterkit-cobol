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
    /// <summary>
    /// String <c>FileName</c> is used in the parser as a parameter for the <c>ErrorHandler</c> method.
    /// <para>The error handler will use this to fetch the file and get the correct line and column when displaying the error message.</para>
    /// </summary>
    private static string FileName = string.Empty;

    /// <summary>
    /// Stack string <c>SourceId</c> is used in the parser whenever it needs to know the name of the current source unit (The identifier after PROGRAM-ID).
    /// <para>This is used when checking if a variable already exists in the current source unit, and when adding them to the DataItemSymbolTable class's variable table.
    /// The DataItemSymbolTable class is then used to simplify the codegen process of generating data items.</para>
    /// </summary>
    private static readonly Stack<string> SourceId = new();

    /// <summary>
    /// Stack SourceUnit <c>SourceType</c> is used in the parser whenever it needs to know which <c>-ID</c> it is currently parsing.
    /// <para>This is used when handling certain syntax rules for different <c>-ID</c>s, like the <c>"RETURNING data-name"</c> being required for every <c>FUNCTION-ID</c> source unit.</para>
    /// </summary>
    private static readonly Stack<SourceUnit> SourceType = new();

    /// <summary>
    /// CurrentScope <c>CurrentSection</c> is used in the parser whenever it needs to know which section it is currently parsing (WORKING-STORAGE and LOCAL-STORAGE for example).
    /// <para>This is used when handling certain syntax rules for different sections and to add extra context needed for the SymbolTable class's variable table.
    /// This will also be used by the SymbolTable class during codegen to simplify the process to figuring out if a variable is static or not.</para>
    /// </summary>
    private static CurrentScope CurrentSection;

    /// <summary>
    /// List of Tokens <c>TokenList</c>: This is the main data structure that the parser will be iterating through.
    /// <para>The parser expects a list of already preprocessed and classified COBOL tokens in the form of full COBOL words (CALL, END-IF, COMPUTE for example)</para>
    /// </summary>
    private static List<Token> TokenList = new();

    /// <summary>
    /// Int <c>Index</c>: This is the index of the current token, used by most helper methods including Continue, Current and Lookahead.
    /// <para>The index should only move forwards, but if the previous token is needed you can use the Lookahead and LookaheadEquals methods with a negative integer parameter</para>
    /// </summary>
    private static int Index;

    /// <summary>
    /// Int <c>FileIndex</c>: This is the index of the current file name, used by the parser to point to the correct file name when showing error messages.
    /// <para>The file index should only move forwards.</para>
    /// </summary>
    private static int FileIndex;

    /// <summary>
    /// Otterkit COBOL Syntax Analyzer
    /// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
    /// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
    /// </summary>
    public static List<Token> Analyze(List<Token> tokenList, string fileName)
    {
        FileName = fileName;
        TokenList = tokenList;

        // Call the parser's main method
        // This should only return when the parser reaches the true EOF token
        Source();

        // If a parsing error has occured, terminate the compilation process.
        // We do not want the compiler to continue when the source code is not valid.
        if (ErrorHandler.Error) ErrorHandler.Terminate("parsing");

        // Return parsed list of tokens.
        return TokenList;

        // Source() is the main method of the parser.
        // It's responsible for parsing COBOL divisions until the EOF token.
        // If EOF was not returned as the last Token in the list then,
        // the parser has not finished reading through the list of tokens correctly.
        void Source()
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
                if (CurrentEquals("PROCEDURE")) PROCEDURE();
            }
            else if (SourceType.Peek() == SourceUnit.Class)
            {
                FactoryObject();
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

            if (CurrentEquals("EOF") && Index < TokenList.Count - 1)
            {
                FileName = OtterkitCompiler.Options.FileNames[FileIndex++];

                Continue();
                Source();
            }
        }


        // Method responsible for parsing the IDENTIFICATION DIVISION.
        // That includes PROGRAM-ID, FUNCTION-ID, CLASS-ID, METHOD-ID, INTERFACE-ID, OBJECT, FACTORY and OPTIONS paragraphs.
        // It is also responsible for showing appropriate error messages when an error occurs in the IDENTIFICATION DIVISION.
        void IDENTIFICATION()
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

        void Options()
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
        void ProgramId()
        {
            Token ProgramIdentifier;

            Expected("PROGRAM-ID");
            Expected(".");

            ProgramIdentifier = Current();
            SourceId.Push(ProgramIdentifier.value);
            SourceType.Push(SourceUnit.Program);
            CurrentSection = CurrentScope.ProgramId;

            Identifier();
            if (CurrentEquals("AS"))
            {
                Expected("AS");
                SourceId.Pop();
                String();
                SourceId.Push(Lookahead(-1).value);
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
                    ErrorHandler.Parser.Report(FileName, ProgramIdentifier, ErrorType.General, """
                    Invalid prototype. Program prototypes cannot be defined as common, initial or recursive.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, ProgramIdentifier);
                }

                if (isInitial && isRecursive)
                {
                    ErrorHandler.Parser.Report(FileName, ProgramIdentifier, ErrorType.General, """
                    Invalid program definition. Initial programs cannot be defined as recursive.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, ProgramIdentifier);
                }

                if (!isPrototype) Optional("PROGRAM");
            }

            SymbolTable.AddSymbol($"{SourceId.Peek()}", SymbolType.SourceUnitSignature);
            Expected(".", """
            Missing separator period at the end of this program definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        void FunctionId()
        {
            Expected("FUNCTION-ID");
            Expected(".");

            SourceId.Push(Current().value);
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

            SymbolTable.AddSymbol($"{SourceId.Peek()}", SymbolType.SourceUnitSignature);
            Expected(".", """
            Missing separator period at the end of this function definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        void ClassId()
        {
            Expected("CLASS-ID");
            Expected(".");

            SourceId.Push(Current().value);
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
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The INHERITS FROM clause must contain at least one class or object name.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            if (CurrentEquals("USING"))
            {
                Expected("USING");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The USING clause must contain at least one parameter.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            SymbolTable.AddSymbol($"{SourceId.Peek()}", SymbolType.SourceUnitSignature);
            Expected(".", """
            Missing separator period at the end of this class definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "FACTORY", "OBJECT");
        }

        void InterfaceId()
        {
            Expected("INTERFACE-ID");
            Expected(".");

            SourceId.Push(Current().value);
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
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The INHERITS FROM clause must contain at least one class or object name.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            if (CurrentEquals("USING"))
            {
                Expected("USING");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The USING clause must contain at least one parameter.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();
            }

            SymbolTable.AddSymbol($"{SourceId.Peek()}", SymbolType.SourceUnitSignature);
            Expected(".", """
            Missing separator period at the end of this interface definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "FACTORY", "OBJECT");
        }

        void MethodId()
        {
            Expected("METHOD-ID");
            Expected(".");

            CurrentSection = CurrentScope.MethodId;
            var currentSource = SourceType.Peek();
            var currentId = SourceId.Peek();

            if (currentSource != SourceUnit.Interface && CurrentEquals("GET"))
            {
                Expected("GET");
                Expected("PROPERTY");
                SourceId.Push($"GET {Current().value}");
                SourceType.Push(SourceUnit.MethodGetter);

                SymbolTable.AddSymbol($"{currentId}->{SourceId.Peek()}", SymbolType.SourceUnitSignature);

                Identifier();

            }
            else if (currentSource != SourceUnit.Interface && CurrentEquals("SET"))
            {
                Expected("SET");
                Expected("PROPERTY");

                SourceId.Push($"SET {Current().value}");
                SourceType.Push(SourceUnit.MethodSetter);

                SymbolTable.AddSymbol($"{currentId}->{SourceId.Peek()}", SymbolType.SourceUnitSignature);

                Identifier();
            }
            else // If not a getter or a setter
            {
                Identifier();
                if (CurrentEquals("AS"))
                {
                    Expected("AS");
                    String();
                }
                SourceId.Push(Lookahead(-1).value);

                if (currentSource == SourceUnit.Interface)
                {
                    SourceType.Push(SourceUnit.MethodPrototype);
                }
                else
                {
                    SourceType.Push(SourceUnit.Method);
                }

                SymbolTable.AddSymbol($"{currentId}->{SourceId.Peek()}", SymbolType.SourceUnitSignature);
            }

            if (CurrentEquals("OVERRIDE")) Expected("OVERRIDE");

            if (CurrentEquals("IS", "FINAL"))
            {
                Optional("IS");
                Expected("FINAL");
            }

            Expected(".", """
            Missing separator period at the end of this method definition
            """, -1, "OPTION", "ENVIRONMENT", "DATA", "PROCEDURE");
        }

        void Factory()
        {
            Expected("FACTORY");
            Expected(".");

            SourceType.Push(SourceUnit.Factory);

            if (CurrentEquals("IMPLEMENTS"))
            {
                Expected("IMPLEMENTS");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The IMPLEMENTS clause must contain at least one interface name.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();

                Expected(".");
            }
        }

        void Object()
        {
            Expected("OBJECT");
            Expected(".");

            SourceType.Push(SourceUnit.Object);

            if (CurrentEquals("IMPLEMENTS"))
            {
                Expected("IMPLEMENTS");
                if (!CurrentEquals(TokenType.Identifier))
                {
                    ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                    The IMPLEMENTS clause must contain at least one interface name.
                    """);
                    ErrorHandler.Parser.PrettyError(FileName, Current());
                }

                Identifier();
                while (CurrentEquals(TokenType.Identifier)) Identifier();

                Expected(".");
            }
        }

        void FactoryObject()
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

        void InterfaceProcedure()
        {
            if (CurrentEquals("PROCEDURE"))
            {
                Expected("PROCEDURE");
                Expected("DIVISION");
                Expected(".", """
                Missing separator period at the end of this PROCEDURE DIVISION header, every division header must end with a separator period
                """, -1, "METHOD-ID", "END");

                while (CurrentEquals("METHOD-ID") || CurrentEquals("IDENTIFICATION") && LookaheadEquals(3, "METHOD-ID"))
                {
                    IDENTIFICATION();

                    if (CurrentEquals("ENVIRONMENT")) ENVIRONMENT();

                    if (CurrentEquals("DATA")) DATA();

                    if (CurrentEquals("PROCEDURE")) PROCEDURE();

                    EndMarker();
                }
            }
        }

        // Method responsible for parsing the ENVIRONMENT DIVISION.
        // That includes the CONFIGURATION and the INPUT-OUTPUT sections.
        // It is also responsible for showing appropriate error messages when an error occurs in the ENVIRONMENT DIVISION.
        void ENVIRONMENT()
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

        void REPOSITORY()
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
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING clause must contain at least one class, object or interface name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
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
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING clause must contain at least one class, object or interface name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
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
        void PROCEDURE()
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
                    if (CurrentEquals("BY") && !LookaheadEquals(1, "VALUE", "REFERENCE"))
                    {
                        ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                        The USING BY clause in the procedure division header must be followed by "VALUE" or "REFERENCE"
                        """);
                        ErrorHandler.Parser.PrettyError(FileName, Current());

                        CombinedAnchorPoint(TokenContext.IsStatement, "RETURNING", ".");
                    }

                    if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && LookaheadEquals(1, "REFERENCE"))
                    {
                        var optional = false;
                        Optional("BY");
                        Expected("REFERENCE");

                        if (CurrentEquals("OPTIONAL"))
                        {
                            optional = true;
                            Expected("OPTIONAL");
                        }

                        if (!CurrentEquals(TokenType.Identifier))
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY REFERENCE clause must contain at least one data item name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }
                        
                        SourceUnitSignature signature;

                        if (SourceType.Peek() is SourceUnit.Method or SourceUnit.MethodPrototype or SourceUnit.MethodGetter or SourceUnit.MethodSetter)
                        {
                            var currentId = SourceId.Pop();
                            signature = SymbolTable.GetSourceUnit($"{SourceId.Peek()}->{currentId}");
                            SourceId.Push(currentId);
                        }
                        else
                        {
                            signature = SymbolTable.GetSourceUnit($"{SourceId.Peek()}");
                        }

                        signature.Parameters.Add(Current().value);
                        signature.IsOptional.Add(optional);
                        signature.IsByRef.Add(true);

                        Identifier();
                        while (CurrentEquals(TokenType.Identifier) || CurrentEquals("OPTIONAL"))
                        {
                            optional = false;
                            if (CurrentEquals("OPTIONAL"))
                            {
                                optional = true;
                                Expected("OPTIONAL");
                            }
                            signature.Parameters.Add(Current().value);
                            signature.IsOptional.Add(optional);
                            signature.IsByRef.Add(true);
                            Identifier();
                        }
                    }

                    if (CurrentEquals("VALUE") || CurrentEquals("BY") && LookaheadEquals(1, "VALUE"))
                    {
                        Optional("BY");
                        Expected("VALUE");
                        if (!CurrentEquals(TokenType.Identifier))
                        {
                            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                            The USING BY VALUE clause must contain at least one data item name.
                            """);
                            ErrorHandler.Parser.PrettyError(FileName, Current());
                        }
                        
                        SourceUnitSignature signature;

                        if (SourceType.Peek() is SourceUnit.Method or SourceUnit.MethodPrototype or SourceUnit.MethodGetter or SourceUnit.MethodSetter)
                        {
                            var currentId = SourceId.Pop();
                            signature = SymbolTable.GetSourceUnit($"{SourceId.Peek()}->{currentId}");
                            SourceId.Push(currentId);
                        }
                        else
                        {
                            signature = SymbolTable.GetSourceUnit($"{SourceId.Peek()}");
                        }

                        signature.Parameters.Add(Current().value);
                        signature.IsOptional.Add(false);
                        signature.IsByRef.Add(false);
                        Identifier();
                        while (CurrentEquals(TokenType.Identifier))
                        {
                            signature.Parameters.Add(Current().value);
                            signature.IsOptional.Add(false);
                            signature.IsByRef.Add(false);
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

            Expected(".", """
            Missing separator period at the end of this PROCEDURE DIVISION header, every division header must end with a separator period
            """, -1, TokenContext.IsStatement);

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
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The procedure division of function, program and method prototypes must not contain any statements, sections or paragraphs
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());

                AnchorPoint("END");
            }

        }

        // This method is part of the PROCEDURE DIVISION parsing. It's used to parse the "RETURNING" data item specified in
        // the PROCEDURE DIVISION header. It's separate from the previous method because its code is needed more than once.
        // COBOL user-defined functions should always return a data item.
        void ReturningDataName()
        {
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler.Parser.Report(FileName, Lookahead(-1), ErrorType.General, """
                Missing returning data item after this RETURNING definition.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Lookahead(-1));
                return;
            }

            SourceUnitSignature signature;

            if (SourceType.Peek() is SourceUnit.Method or SourceUnit.MethodPrototype or SourceUnit.MethodGetter or SourceUnit.MethodSetter)
            {
                var currentId = SourceId.Pop();
                signature = SymbolTable.GetSourceUnit($"{SourceId.Peek()}->{currentId}");
                SourceId.Push(currentId);
            }
            else
            {
                signature = SymbolTable.GetSourceUnit($"{SourceId.Peek()}");
            }

            signature.Returning = Current().value;
            Identifier();
        }

        void EndMarker()
        {
            SourceUnit currentSource = SourceType.Peek();

            if (currentSource != SourceUnit.Program && !CurrentEquals("END") || currentSource == SourceUnit.Program && (!CurrentEquals(TokenType.EOF) && !CurrentEquals("END")))
            {
                string errorMessageChoice = currentSource switch
                {
                    SourceUnit.Program or SourceUnit.ProgramPrototype => """
                    Missing END PROGRAM marker. If another source unit is present after the end of a program or program prototype, the program must contain an END marker.
                    """,

                    SourceUnit.Function or SourceUnit.FunctionPrototype => """
                    Missing END FUNCTION marker. User-defined functions and function prototypes must always end with an END FUNCTION marker.
                    """,

                    SourceUnit.Method or SourceUnit.MethodPrototype or SourceUnit.MethodGetter or SourceUnit.MethodSetter => """
                    Missing END METHOD marker. Method definitions and property getter/setter must always end with an END METHOD marker.
                    """,

                    SourceUnit.Class => """
                    Missing END CLASS marker. Class definitions must always end with an END CLASS marker.
                    """,

                    SourceUnit.Interface => """
                    Missing END INTERFACE marker. Interface definitions must always end with an END INTERFACE marker.
                    """,

                    SourceUnit.Factory or SourceUnit.Object => """
                    Missing END FACTORY and END OBJECT marker. Factory and object definitions must always end with an END FACTORY and END OBJECT marker.
                    """,

                    _ => throw new UnreachableException()
                };

                ErrorHandler.Parser.Report(FileName, Lookahead(-1), ErrorType.General, errorMessageChoice);
                ErrorHandler.Parser.PrettyError(FileName, Lookahead(-1));
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
                    Identifier(SourceId.Pop());
                    Expected(".", """
                    Missing separator period at the end of this END PROGRAM definition
                    """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Function:
                case SourceUnit.FunctionPrototype:
                    SourceType.Pop();

                    Expected("END");
                    Expected("FUNCTION");
                    Identifier(SourceId.Pop());
                    Expected(".", """
                    Missing separator period at the end of this END FUNCTION definition
                    """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Method:
                case SourceUnit.MethodPrototype:
                case SourceUnit.MethodGetter:
                case SourceUnit.MethodSetter:
                    SourceType.Pop();

                    Expected("END");
                    Expected("METHOD");
                    if (currentSource is SourceUnit.Method or SourceUnit.MethodPrototype)
                        Identifier(SourceId.Pop());

                    if (currentSource is SourceUnit.MethodGetter or SourceUnit.MethodSetter)
                        SourceId.Pop();

                    Expected(".", """
                    Missing separator period at the end of this END METHOD definition
                    """, -1, "IDENTIFICATION", "METHOD-ID", "OBJECT", "FACTORY");
                    break;

                case SourceUnit.Class:
                    SourceType.Pop();

                    Expected("END");
                    Expected("CLASS");
                    Identifier(SourceId.Pop());
                    Expected(".", """
                    Missing separator period at the end of this END CLASS definition
                    """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Interface:
                    SourceType.Pop();

                    Expected("END");
                    Expected("INTERFACE");
                    Identifier(SourceId.Pop());
                    Expected(".", """
                    Missing separator period at the end of this END INTERFACE definition
                    """, -1, "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Factory:
                    SourceType.Pop();

                    Expected("END");
                    Expected("FACTORY");
                    Expected(".", """
                    Missing separator period at the end of this END FACTORY definition
                    """, -1, "OBJECT", "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

                case SourceUnit.Object:
                    SourceType.Pop();

                    Expected("END");
                    Expected("OBJECT");
                    Expected(".", """
                    Missing separator period at the end of this END FACTORY definition
                    """, -1, "END", "IDENTIFICATION", "PROGRAM-ID", "FUNCTION-ID", "CLASS-ID", "INTERFACE-ID");
                    break;

            }
        }
    }
}
