using static Otterkit.Types.TokenHandling;
using static Otterkit.Types.CompilerContext;
using Otterkit.Types;

namespace Otterkit.Analyzers;

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

                AnchorPoint("PROGRAM-ID FUNCTION-ID ENVIRONMENT DATA PROCEDURE");
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
            Choice("NATIVE STANDARD-BINARY STANDARD-DECIMAL");

            shouldHavePeriod = true;
        }

        if (CurrentEquals("DEFAULT"))
        {
            Expected("DEFAULT");
            Expected("ROUNDED");
            Optional("MODE");
            Optional("IS");

            Choice("AWAY-FROM-ZERO NEAREST-AWAY-FROM-ZERO NEAREST-EVEN NEAREST-TOWARD-ZERO PROHIBITED TOWARD-GREATER TOWARD-LESSER TRUNCATION");

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

            Choice("HIGH-ORDER-LEFT HIGH-ORDER-RIGHT");
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
                Literals.String();
            }
            else if (CurrentEquals("BINARY"))
            {
                Expected("BINARY");
                Expected("ZEROS");
            }
            else
            {
                Choice("SPACES LOW-VALUES HIGH-VALUES");
            }
        }

        if (CurrentEquals("INTERMEDIATE"))
        {
            Expected("INTERMEDIATE");
            Expected("ROUNDING");

            Choice("NEAREST-AWAY-FROM-ZERO NEAREST-EVEN PROHIBITED TRUNCATION");
        }

        if (shouldHavePeriod) Expected(".");
    }

    private static void InitializeChoices(bool localExists = false, bool screenExists = false, bool workingExists = false)
    {
        if (CurrentEquals("LOCAL-STORAGE"))
        {
            if (localExists)
            {
                ErrorHandler
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
                ErrorHandler
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
                ErrorHandler
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
    // This is where SourceId and SourceTypeStack get their values for a COBOL source unit.

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

        ErrorHandler
        .Build(ErrorType.Analyzer, ConsoleColor.Red, 85, """
            Missing source unit definition.
            """)
        .WithSourceLine(Current(), """
            Expected a source unit id definition.
            """)
        .WithNote("""
            The identification header is optional but every source unit must still have an ID.
            """)
        .CloseError();

        AnchorPoint("OPTIONS ENVIRONMENT DATA PROCEDURE");
    }

    private static void ProgramId()
    {
        Expected("PROGRAM-ID");
        Expected(".");

        ActiveUnits.Push(Current());

        SourceTypes.Push(UnitKind.Program);

        ActiveScope = SourceScope.ProgramId;

        References.GlobalName(false);

        if (CurrentEquals("AS"))
        {
            Expected("AS");

            ActiveUnits.Pop();

            Literals.String();

            ActiveUnits.Push(Peek(-1));
        }

        if (CurrentEquals("IS COMMON INITIAL RECURSIVE PROTOTYPE"))
        {
            bool isCommon = false;
            bool isInitial = false;
            bool isPrototype = false;
            bool isRecursive = false;

            Optional("IS");

            while (CurrentEquals("COMMON INITIAL RECURSIVE PROTOTYPE"))
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

                    SourceTypes.Pop();

                    SourceTypes.Push(UnitKind.ProgramPrototype);

                    isPrototype = true;
                }
            }

            if (isPrototype && (isCommon || isInitial || isRecursive))
            {
                ErrorHandler
                .Build(ErrorType.Syntax, ConsoleColor.Red, 55, """
                    Invalid program prototype definition.
                    """)
                .WithSourceLine(ActiveUnits.Peek(), """
                    Program prototypes cannot be defined as common, initial or recursive.
                    """)
                .CloseError();
            }

            if (isInitial && isRecursive)
            {
                ErrorHandler
                .Build(ErrorType.Syntax, ConsoleColor.Red, 55, """
                    Invalid program definition.
                    """)
                .WithSourceLine(ActiveUnits.Peek(), """
                    Initial programs cannot be defined as recursive.
                    """)
                .CloseError();
            }

            if (!isPrototype) Optional("PROGRAM");
        }

        if (!IsResolutionPass)
        {
            var signature = new CallableUnit(ActiveUnits.Peek(), SourceTypes.Peek());

            ActiveNames.TryAdd(ActiveUnits.Peek(), signature);

            ActiveCallable = signature;
        }

        if (IsResolutionPass)
        {
            ActiveCallable = ActiveNames.Fetch<CallableUnit>(ActiveUnits.Peek());
        }

        if (!Expected(".", false))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                Program definition, missing separator period.
                """)
            .WithSourceLine(Peek(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION ENVIRONMENT DATA PROCEDURE");
        }
    }

    private static void FunctionId()
    {
        Expected("FUNCTION-ID");
        Expected(".");

        SourceTypes.Push(UnitKind.Function);

        ActiveScope = SourceScope.FunctionId;

        References.Identifier();

        if (CurrentEquals("AS"))
        {
            Expected("AS");
            Literals.String();
        }

        if (CurrentEquals("IS PROTOTYPE"))
        {
            Optional("IS");
            Expected("PROTOTYPE");
            SourceTypes.Pop();
            SourceTypes.Push(UnitKind.FunctionPrototype);
        }

        if (!IsResolutionPass)
        {
            var signature = new CallableUnit(ActiveUnits.Peek(), SourceTypes.Peek());

            ActiveNames.TryAdd(ActiveUnits.Peek(), signature);

            ActiveCallable = signature;
        }

        if (IsResolutionPass)
        {
            ActiveCallable = ActiveNames.Fetch<CallableUnit>(ActiveUnits.Peek());
        }

        if (!Expected(".", false))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                Function definition, missing separator period.
                """)
            .WithSourceLine(Peek(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION ENVIRONMENT DATA PROCEDURE");
        }
    }

    private static void ClassId()
    {
        Expected("CLASS-ID");
        Expected(".");

        ActiveUnits.Push(Current());

        SourceTypes.Push(UnitKind.Class);
        ActiveScope = SourceScope.ClassId;

        References.Identifier();

        if (CurrentEquals("AS"))
        {
            Expected("AS");
            Literals.String();
        }

        if (CurrentEquals("IS FINAL"))
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
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 60, """
                    Class definition, missing INHERITS class.
                    """)
                .WithSourceLine(Peek(-1), """
                    The INHERITS phrase must contain a class name.
                    """)
                .CloseError();
            }

            References.Identifier();
        }

        if (CurrentEquals("USING"))
        {
            Expected("USING");
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 60, """
                    Class definition, missing using parameter.
                    """)
                .WithSourceLine(Peek(-1), """
                    The USING phrase must contain at least one parameter.
                    """)
                .CloseError();
            }

            References.Identifier();
            while (CurrentEquals(TokenType.Identifier)) References.Identifier();
        }

        if (!IsResolutionPass)
        {
            var signature = new ClassUnit(ActiveUnits.Peek(), SourceTypes.Peek());

            ActiveNames.TryAdd(ActiveUnits.Peek(), signature);
        }

        if (!Expected(".", false))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                Class definition, missing separator period.
                """)
            .WithSourceLine(Peek(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION ENVIRONMENT DATA FACTORY OBJECT");
        }
    }

    private static void InterfaceId()
    {
        Expected("INTERFACE-ID");
        Expected(".");

        ActiveUnits.Push(Current());

        SourceTypes.Push(UnitKind.Interface);
        ActiveScope = SourceScope.InterfaceId;

        References.Identifier();

        if (CurrentEquals("AS"))
        {
            Expected("AS");
            Literals.String();
        }

        if (CurrentEquals("INHERITS"))
        {
            Expected("INHERITS");
            Optional("FROM");

            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 60, """
                    Interface definition, missing INHERITS class.
                    """)
                .WithSourceLine(Peek(-1), """
                    The INHERITS phrase must contain at least one interface name.
                    """)
                .CloseError();
            }

            References.Identifier();
            while (CurrentEquals(TokenType.Identifier)) References.Identifier();
        }

        if (CurrentEquals("USING"))
        {
            Expected("USING");
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 60, """
                    Interface definition, missing using parameter.
                    """)
                .WithSourceLine(Peek(-1), """
                    The USING phrase must contain at least one parameter.
                    """)
                .CloseError();
            }

            References.Identifier();
            while (CurrentEquals(TokenType.Identifier)) References.Identifier();
        }

        if (!IsResolutionPass)
        {
            var signature = new InterfaceUnit(ActiveUnits.Peek(), SourceTypes.Peek());

            ActiveNames.TryAdd(ActiveUnits.Peek(), signature);
        }

        if (!Expected(".", false))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                Interface definition, missing separator period.
                """)
            .WithSourceLine(Peek(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION ENVIRONMENT DATA PROCEDURE");
        }
    }

    private static void MethodId()
    {
        if (SourceTypes.Peek() is not (UnitKind.Object or UnitKind.Factory or UnitKind.Interface))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 90, """
                Misplaced source unit definition.
                """)
            .WithSourceLine(Current(), """
                Method definitions can only be specified inside class or interface definitions.
                """)
            .CloseError();
        }

        Expected("METHOD-ID");
        Expected(".");

        ActiveScope = SourceScope.MethodId;

        if (SourceTypes.Peek() != UnitKind.Interface && CurrentEquals("GET"))
        {
            Expected("GET");
            Expected("PROPERTY");

            ActiveUnits.Push(Current());
            SourceTypes.Push(UnitKind.MethodGetter);

            References.Identifier();

        }
        else if (SourceTypes.Peek() != UnitKind.Interface && CurrentEquals("SET"))
        {
            Expected("SET");
            Expected("PROPERTY");

            ActiveUnits.Push(Current());
            SourceTypes.Push(UnitKind.MethodSetter);

            References.Identifier();
        }
        else // If not a getter or a setter
        {
            ActiveUnits.Push(Current());

            References.Identifier();

            if (CurrentEquals("AS"))
            {
                Expected("AS");
                Literals.String();
            }

            if (SourceTypes.Peek() == UnitKind.Interface)
            {
                SourceTypes.Push(UnitKind.MethodPrototype);
            }
            else
            {
                SourceTypes.Push(UnitKind.Method);
            }
        }

        if (CurrentEquals("OVERRIDE")) Expected("OVERRIDE");

        if (CurrentEquals("IS FINAL"))
        {
            Optional("IS");
            Expected("FINAL");
        }

        if (!Expected(".", false))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 25, """
                Interface definition, missing separator period.
                """)
            .WithSourceLine(Peek(-1), """
                Expected a separator period '. ' after this token.
                """)
            .WithNote("""
                Every source unit definition must end with a separator period.
                """)
            .CloseError();

            AnchorPoint("OPTION ENVIRONMENT DATA PROCEDURE");
        }

        // We're returning during a resolution pass
        if (IsResolutionPass) return;

        // Because we don't want to run this again during it:
        if (SourceTypes.Peek() is UnitKind.Interface)
        {
            var parentInterface = ActiveNames.Fetch<InterfaceUnit>(ActiveUnits.Peek());

            var methodPrototype = new CallableUnit(ActiveUnits.Peek(), SourceTypes.Peek());

            parentInterface.Methods.Add(methodPrototype);

            ActiveCallable = methodPrototype;
        }

        var parentClass = ActiveNames.Fetch<ClassUnit>(ActiveUnits.Peek());

        var method = new CallableUnit(ActiveUnits.Peek(), SourceTypes.Peek());

        if (SourceTypes.Peek() is UnitKind.Object)
        {
            parentClass.ObjectMethods.Add(method);
        }

        if (SourceTypes.Peek() is UnitKind.Factory)
        {
            parentClass.FactoryMethods.Add(method);
        }

        ActiveCallable = method;
    }

    private static void Factory()
    {
        if (SourceTypes.Peek() is not UnitKind.Class)
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 90, """
                Misplaced source unit definition.
                """)
            .WithSourceLine(Current(), """
                Factory definitions can only be specified inside class definitions.
                """)
            .CloseError();
        }

        Expected("FACTORY");
        Expected(".");

        SourceTypes.Push(UnitKind.Factory);

        if (CurrentEquals("IMPLEMENTS"))
        {
            Expected("IMPLEMENTS");
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 95, """
                    Missing implements interfaces.
                    """)
                .WithSourceLine(Peek(-1), """
                    The IMPLEMENTS phrase must contain at least one interface name.
                    """)
                .CloseError();
            }

            References.Identifier();

            while (CurrentEquals(TokenType.Identifier)) References.Identifier();

            Expected(".");
        }
    }

    private static void Object()
    {
        if (SourceTypes.Peek() is not UnitKind.Class)
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 90, """
                Misplaced source unit definition.
                """)
            .WithSourceLine(Current(), """
                Object definitions can only be specified inside class definitions.
                """)
            .CloseError();
        }

        Expected("OBJECT");
        Expected(".");

        SourceTypes.Push(UnitKind.Object);

        if (CurrentEquals("IMPLEMENTS"))
        {
            Expected("IMPLEMENTS");
            if (!CurrentEquals(TokenType.Identifier))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 95, """
                    Missing implements interfaces.
                    """)
                .WithSourceLine(Peek(-1), """
                    The IMPLEMENTS phrase must contain at least one interface name.
                    """)
                .CloseError();
            }

            References.Identifier();

            while (CurrentEquals(TokenType.Identifier)) References.Identifier();

            Expected(".");
        }
    }
}
