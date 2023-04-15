using static Otterkit.SourceAnalyzer.TokenHandling;

namespace Otterkit.SourceAnalyzer;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static class IdentificationDivision
{
    // Method responsible for parsing the IDENTIFICATION DIVISION.
    // That includes PROGRAM-ID, FUNCTION-ID, CLASS-ID, METHOD-ID, INTERFACE-ID, OBJECT, FACTORY and OPTIONS paragraphs.
    // It is also responsible for showing appropriate error messages when an error occurs in the IDENTIFICATION DIVISION.
    public static void Parse()
    {
        if (CurrentEquals("IDENTIFICATION"))
        {
            Expected("IDENTIFICATION");
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

                AnchorPoint("PROGRAM-ID", "FUNCTION-ID", "ENVIRONMENT", "DATA", "PROCEDURE");
            }
        }
        
        IdDefinitions();

        if (CurrentEquals("OPTIONS")) 
            Options();
    }

    private static void Options()
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

        if (CurrentEquals("FLOAT-BINARY"))
        {
            Expected("FLOAT-BINARY");
            Optional("DEFAULT");
            Optional("IS");

            Choice("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT");
        }

        if (CurrentEquals("FLOAT-DECIMAL"))
        {
            Expected("FLOAT-DECIMAL");
            Optional("DEFAULT");
            Optional("IS");

            Common.EncodingEndianness();
        }

        if (CurrentEquals("INITIALIZE"))
        {
            Expected("INITIALIZE");

            InitializeChoices();
            Optional("SECTION");
            Optional("TO");

            if (CurrentEquals(TokenType.String))
            {
                StringLiteral();
            }
            else if (CurrentEquals("BINARY"))
            {
                Expected("BINARY");
                Expected("ZEROS");
            }
            else
            {
                Choice("SPACES","LOW-VALUES", "HIGH-VALUES");
            }
        }

        if (CurrentEquals("INTERMEDIATE"))
        {
            Expected("INTERMEDIATE");
            Expected("ROUNDING");
            Choice(
                "NEAREST-AWAY-FROM-ZERO",
                "NEAREST-EVEN",
                "PROHIBITED",
                "TRUNCATION"
            );
        }

        if (shouldHavePeriod) Expected(".");
    }

    private static void InitializeChoices(bool localExists = false, bool screenExists = false, bool workingExists = false)
    {        
        if (CurrentEquals("LOCAL-STORAGE"))
        {
            if (localExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 134, """
                    Initialize phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    A section can only be specified once in this phrase.
                    """)
                .CloseError();
            }
            localExists = true;

            Expected("LOCAL-STORAGE");

            InitializeChoices(localExists, screenExists, workingExists);

            return;
        }

        if (CurrentEquals("SCREEN"))
        {
            if (screenExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 134, """
                    Initialize phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    A section can only be specified once in this phrase.
                    """)
                .CloseError();
            }
            screenExists = true;

            Expected("SCREEN");

            InitializeChoices(localExists, screenExists, workingExists);

            return;
        }

        if (CurrentEquals("WORKING-STORAGE"))
        {
            if (workingExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 134, """
                    Initialize phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    A section can only be specified once in this phrase.
                    """)
                .CloseError();
            }
            workingExists = true;

            Expected("WORKING-STORAGE");

            InitializeChoices(localExists, screenExists, workingExists);

            return;
        }

        if (CurrentEquals("ALL"))
        {
            Expected("ALL");
        }
    }


    // The following methods are responsible for parsing the -ID paragraph.
    // That includes the program, user-defined function, method, class, interface, factory or object identifier that should be specified right after.
    // This is where SourceId and CompilerContext.SourceTypeStack get their values for a COBOL source unit.

    private static void IdDefinitions()
    {
        if (CurrentEquals("CLASS-ID")) 
        {
            ClassId();
            return;
        }

        if (CurrentEquals("PROGRAM-ID")) 
        {
            ProgramId();
            return;
        }

        if (CurrentEquals("FUNCTION-ID")) 
        {
            FunctionId();
            return;
        }

        if (CurrentEquals("INTERFACE-ID")) 
        {
            InterfaceId(); 
            return;
        }

        if (CurrentEquals("METHOD-ID")) 
        {
            MethodId();
            return;
        }

        if (CurrentEquals("FACTORY")) 
        {
            Factory();
            return;
        }

        if (CurrentEquals("OBJECT")) 
        {
            Object();
            return;
        }

        Error
        .Build(ErrorType.Analyzer, ConsoleColor.Red, 85,"""
            Missing source unit definition.
            """)
        .WithSourceLine(Current(), """
            Expected a source unit id definition.
            """)
        .WithNote("""
            The identification header is optional but every source unit must still have an ID.
            """)
        .CloseError();

        AnchorPoint("OPTIONS", "ENVIRONMENT", "DATA", "PROCEDURE");
    }

    private static void ProgramId()
    {
        Expected("PROGRAM-ID");
        Expected(".");

        CompilerContext.ActiveUnits.Push(Current());
        CompilerContext.SourceTypes.Push(SourceUnit.Program);
        CompilerContext.ActiveScope = CurrentScope.ProgramId;

        Identifier();
        if (CurrentEquals("AS"))
        {
            Expected("AS");
            CompilerContext.ActiveUnits.Pop();
            StringLiteral();
            CompilerContext.ActiveUnits.Push(Lookahead(-1));
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
                    CompilerContext.SourceTypes.Pop();
                    CompilerContext.SourceTypes.Push(SourceUnit.ProgramPrototype);
                    isPrototype = true;
                }
            }

            if (isPrototype && (isCommon || isInitial || isRecursive))
            {
                Error
                .Build(ErrorType.Syntax, ConsoleColor.Red, 55,"""
                    Invalid program prototype definition.
                    """)
                .WithSourceLine(CompilerContext.ActiveUnits.Peek(), """
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
                .WithSourceLine(CompilerContext.ActiveUnits.Peek(), """
                    Initial programs cannot be defined as recursive.
                    """)
                .CloseError();
            }

            if (!isPrototype) Optional("PROGRAM");
        }

        if (!CompilerContext.IsResolutionPass)
        {
            var signature = new CallableSignature(CompilerContext.ActiveUnits.Peek(), CompilerContext.SourceTypes.Peek());

            SymbolTable.TryAddName(CompilerContext.ActiveUnits.Peek().Value, signature);

            CompilerContext.ActiveSignature = signature;
        }

        if (CompilerContext.IsResolutionPass)
        {
            CompilerContext.ActiveSignature = SymbolTable.GetSignature<CallableSignature>(CompilerContext.ActiveUnits.Peek().Value);
        }

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

    private static void FunctionId()
    {
        Expected("FUNCTION-ID");
        Expected(".");

        CompilerContext.SourceTypes.Push(SourceUnit.Function);
        CompilerContext.ActiveScope = CurrentScope.FunctionId;

        Identifier();

        if (CurrentEquals("AS"))
        {
            Expected("AS");
            StringLiteral();
        }

        if (CurrentEquals("IS", "PROTOTYPE"))
        {
            Optional("IS");
            Expected("PROTOTYPE");
            CompilerContext.SourceTypes.Pop();
            CompilerContext.SourceTypes.Push(SourceUnit.FunctionPrototype);
        }

        if (!CompilerContext.IsResolutionPass)
        {
            var signature = new CallableSignature(CompilerContext.ActiveUnits.Peek(), CompilerContext.SourceTypes.Peek());

            SymbolTable.TryAddName(CompilerContext.ActiveUnits.Peek().Value, signature);

            CompilerContext.ActiveSignature = signature;
        }

        if (CompilerContext.IsResolutionPass)
        {
            CompilerContext.ActiveSignature = SymbolTable.GetSignature<CallableSignature>(CompilerContext.ActiveUnits.Peek().Value);
        }

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

    private static void ClassId()
    {
        Expected("CLASS-ID");
        Expected(".");

        CompilerContext.ActiveUnits.Push(Current());

        CompilerContext.SourceTypes.Push(SourceUnit.Class);
        CompilerContext.ActiveScope = CurrentScope.ClassId;

        Identifier();

        if (CurrentEquals("AS"))
        {
            Expected("AS");
            StringLiteral();
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

        if (!CompilerContext.IsResolutionPass)
        {
            var signature = new ClassSignature(CompilerContext.ActiveUnits.Peek(), CompilerContext.SourceTypes.Peek());

            SymbolTable.TryAddName(CompilerContext.ActiveUnits.Peek().Value, signature);
        }

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

    private static void InterfaceId()
    {
        Expected("INTERFACE-ID");
        Expected(".");
        
        CompilerContext.ActiveUnits.Push(Current());

        CompilerContext.SourceTypes.Push(SourceUnit.Interface);
        CompilerContext.ActiveScope = CurrentScope.InterfaceId;

        Identifier();

        if (CurrentEquals("AS"))
        {
            Expected("AS");
            StringLiteral();
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

        if (!CompilerContext.IsResolutionPass)
        {
            var signature = new InterfaceSignature(CompilerContext.ActiveUnits.Peek(), CompilerContext.SourceTypes.Peek());

            SymbolTable.TryAddName(CompilerContext.ActiveUnits.Peek().Value, signature);
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
    }

    private static void MethodId()
    {
        var sourceTypes = CompilerContext.SourceTypes;
        var activeUnits = CompilerContext.ActiveUnits;

        if (sourceTypes.Peek() is not (SourceUnit.Object or SourceUnit.Factory or SourceUnit.Interface))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 90,"""
                Misplaced source unit definition.
                """)
            .WithSourceLine(Current(), """
                Method definitions can only be specified inside class or interface definitions.
                """)
            .CloseError();
        }

        Expected("METHOD-ID");
        Expected(".");

        CompilerContext.ActiveScope = CurrentScope.MethodId;

        if (sourceTypes.Peek() != SourceUnit.Interface && CurrentEquals("GET"))
        {
            Expected("GET");
            Expected("PROPERTY");

            activeUnits.Push(Current());
            sourceTypes.Push(SourceUnit.MethodGetter);

            Identifier();

        }
        else if (sourceTypes.Peek() != SourceUnit.Interface && CurrentEquals("SET"))
        {
            Expected("SET");
            Expected("PROPERTY");

            CompilerContext.ActiveUnits.Push(Current());
            CompilerContext.SourceTypes.Push(SourceUnit.MethodSetter);

            Identifier();
        }
        else // If not a getter or a setter
        {
            CompilerContext.ActiveUnits.Push(Current());

            Identifier();

            if (CurrentEquals("AS"))
            {
                Expected("AS");
                StringLiteral();
            }

            if (sourceTypes.Peek() == SourceUnit.Interface)
            {
                CompilerContext.SourceTypes.Push(SourceUnit.MethodPrototype);
            }
            else
            {
                CompilerContext.SourceTypes.Push(SourceUnit.Method);
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

        // We're returning during a resolution pass
        if (CompilerContext.IsResolutionPass) return;

        // Because we don't want to run this again during it:



        if (sourceTypes.Peek() is SourceUnit.Interface)
        {
            var parentInterface = SymbolTable.GetSignature<InterfaceSignature>(activeUnits.Peek().Value);

            var methodPrototype = new CallableSignature(activeUnits.Peek(), sourceTypes.Peek());

            parentInterface.Methods.Add(methodPrototype);

            CompilerContext.ActiveSignature = methodPrototype;
        }

        var parentClass = SymbolTable.GetSignature<ClassSignature>(activeUnits.Peek().Value);

        var method = new CallableSignature(activeUnits.Peek(), sourceTypes.Peek());

        if (sourceTypes.Peek() is SourceUnit.Object)
        {
            parentClass.ObjectMethods.Add(method);
        }

        if (sourceTypes.Peek() is SourceUnit.Factory)
        {
            parentClass.FactoryMethods.Add(method);
        }
        
        CompilerContext.ActiveSignature = method;
    }

    private static void Factory()
    {
        if (CompilerContext.SourceTypes.Peek() is not SourceUnit.Class)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 90,"""
                Misplaced source unit definition.
                """)
            .WithSourceLine(Current(), """
                Factory definitions can only be specified inside class definitions.
                """)
            .CloseError();
        }

        Expected("FACTORY");
        Expected(".");

        CompilerContext.SourceTypes.Push(SourceUnit.Factory);

        if (CurrentEquals("IMPLEMENTS"))
        {
            Expected("IMPLEMENTS");
            if (!CurrentEquals(TokenType.Identifier))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 95,"""
                    Missing implements interfaces.
                    """)
                .WithSourceLine(Lookahead(-1), """
                    The IMPLEMENTS phrase must contain at least one interface name.
                    """)
                .CloseError();
            }

            Identifier();

            while (CurrentEquals(TokenType.Identifier)) Identifier();

            Expected(".");
        }
    }

    private static void Object()
    {
        if (CompilerContext.SourceTypes.Peek() is not SourceUnit.Class)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 90,"""
                Misplaced source unit definition.
                """)
            .WithSourceLine(Current(), """
                Object definitions can only be specified inside class definitions.
                """)
            .CloseError();
        }

        Expected("OBJECT");
        Expected(".");

        CompilerContext.SourceTypes.Push(SourceUnit.Object);

        if (CurrentEquals("IMPLEMENTS"))
        {
            Expected("IMPLEMENTS");
            if (!CurrentEquals(TokenType.Identifier))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 95,"""
                    Missing implements interfaces.
                    """)
                .WithSourceLine(Lookahead(-1), """
                    The IMPLEMENTS phrase must contain at least one interface name.
                    """)
                .CloseError();
            }

            Identifier();

            while (CurrentEquals(TokenType.Identifier)) Identifier();

            Expected(".");
        }
    }
}
