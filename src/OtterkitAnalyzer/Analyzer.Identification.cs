namespace Otterkit;

/// <summary>
/// Otterkit COBOL Syntax and Semantic Analyzer
/// <para>This parser was built to be easily extensible, with some reusable COBOL parts.</para>
/// <para>It requires a List of Tokens generated from the Lexer and the Token Classifier.</para>
/// </summary>
public static partial class Analyzer
{
    // Method responsible for parsing the IDENTIFICATION DIVISION.
    // That includes PROGRAM-ID, FUNCTION-ID, CLASS-ID, METHOD-ID, INTERFACE-ID, OBJECT, FACTORY and OPTIONS paragraphs.
    // It is also responsible for showing appropriate error messages when an error occurs in the IDENTIFICATION DIVISION.
    private static void IDENTIFICATION()
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

        if (shouldHavePeriod) Expected(".");
    }

    // The following methods are responsible for parsing the -ID paragraph.
    // That includes the program, user-defined function, method, class, interface, factory or object identifier that should be specified right after.
    // This is where SourceId and SourceType get their values for a COBOL source unit.

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

        CurrentId.Push(Current());
        SourceType.Push(SourceUnit.Program);
        CurrentScope = CurrentScope.ProgramId;

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

        if (!IsResolutionPass)
        {
            var signature = new CallableSignature(CurrentId.Peek(), SourceType.Peek());

            SymbolTable.TryAddName(CurrentId.Peek().Value, signature);

            CurrentCallable = signature;
        }

        if (IsResolutionPass)
        {
            CurrentCallable = SymbolTable.GetSignature<CallableSignature>(CurrentId.Peek().Value);
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

        SourceType.Push(SourceUnit.Function);
        CurrentScope = CurrentScope.FunctionId;

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

        if (!IsResolutionPass)
        {
            var signature = new CallableSignature(CurrentId.Peek(), SourceType.Peek());

            SymbolTable.TryAddName(CurrentId.Peek().Value, signature);

            CurrentCallable = signature;
        }

        if (IsResolutionPass)
        {
            CurrentCallable = SymbolTable.GetSignature<CallableSignature>(CurrentId.Peek().Value);
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

        CurrentId.Push(Current());

        SourceType.Push(SourceUnit.Class);
        CurrentScope = CurrentScope.ClassId;

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

        if (!IsResolutionPass)
        {
            var signature = new ClassSignature(CurrentId.Peek(), SourceType.Peek());

            SymbolTable.TryAddName(CurrentId.Peek().Value, signature);
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
        
        CurrentId.Push(Current());

        SourceType.Push(SourceUnit.Interface);
        CurrentScope = CurrentScope.InterfaceId;

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

        if (!IsResolutionPass)
        {
            var signature = new InterfaceSignature(CurrentId.Peek(), SourceType.Peek());

            SymbolTable.TryAddName(CurrentId.Peek().Value, signature);
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
        if (SourceType.Peek() is not (SourceUnit.Object or SourceUnit.Factory or SourceUnit.Interface))
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

        CurrentScope = CurrentScope.MethodId;

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

        // We're returning during a resolution pass
        if (IsResolutionPass) return;

        // Because we don't want to run this again during it:

        if (currentSource is SourceUnit.Interface)
        {
            var parentInterface = SymbolTable.GetSignature<InterfaceSignature>(currentId.Value);

            var methodPrototype = new CallableSignature(CurrentId.Peek(), SourceType.Peek());

            parentInterface.Methods.Add(methodPrototype);

            CurrentCallable = methodPrototype;
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
        
        CurrentCallable = method;
    }

    private static void Factory()
    {
        if (SourceType.Peek() is not SourceUnit.Class)
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

        SourceType.Push(SourceUnit.Factory);

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
        if (SourceType.Peek() is not SourceUnit.Class)
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

        SourceType.Push(SourceUnit.Object);

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
