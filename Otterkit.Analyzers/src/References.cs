using static Otterkit.Types.TokenHandling;
using Otterkit.Types;

namespace Otterkit.Analyzers;

[Flags]
public enum NameTypes
{
    None,
    AlphabetName,
    ClassName = 1 << 1,
    CompilationVariableName = 1 << 2,
    ConditionName = 1 << 3,
    ConstantName = 1 << 4,
    DataName = 1 << 5,
    DirectiveName = 1 << 6,
    DynamicLengthStructureName = 1 << 7,
    FileName = 1 << 8,
    FunctionPrototypeName = 1 << 9,
    IndexName = 1 << 10,
    InterfaceName = 1 << 11,
    LevelNumber = 1 << 12,
    LocaleName = 1 << 13,
    MethodName = 1 << 14,
    MnemonicName = 1 << 15,
    ObjectClassName = 1 << 16,
    OrderingName = 1 << 17,
    ParagraphName = 1 << 18,
    ParameterName = 1 << 19,
    ProgramName = 1 << 20,
    ProgramPrototypeName = 1 << 21,
    PropertyName = 1 << 22,
    RecordKeyName = 1 << 23,
    RecordName = 1 << 24,
    ReportName = 1 << 25,
    ScreenName = 1 << 26,
    SectionName = 1 << 27,
    SymbolicCharacter = 1 << 28,
    TypeName = 1 << 29,
    UserFunctionName = 1 << 30,
}

public static class References
{
    private static bool HasFlag(Enum type, Enum flag)
    {
        return type.HasFlag(flag);
    }

    public static void Name(NameTypes types)
    {

    }

    public static void Qualified(NameTypes types)
    {

    }

    public static void Identifier(IdentifierType allowedTypes = IdentifierType.None)
    {
        // TODO:
        // All Identifiers here need a check from the symbol table.
        // The symbol table itself needs to be refactored to accommodate this.
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 0, """
                Unexpected end of file.
                """)
            .WithSourceLine(Lookahead(-1), $"""
                Expected an identifier after this token.
                """)
            .CloseError();

            return;
        }

        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 1, """
                Unexpected token.
                """)
            .WithSourceLine(Current(), $"""
                Expected a user-defined word (an identifier).
                """)
            .CloseError();

            Continue();
            return;
        }

        if (CurrentEquals("FUNCTION"))
        {
            Expected("FUNCTION");
            if (!HasFlag(allowedTypes, IdentifierType.Function))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected function identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This function call should not be here.
                    """)
                .WithNote("""
                    Function calls must not be used as receiving operands
                    """)
                .CloseError();
            }

            Continue();
            if (CurrentEquals("("))
            {
                // TODO:
                // This needs a check from the symbol table to verify the number and type
                // of the function's parameters.
                Expected("(");
                while (!CurrentEquals(")")) Continue();
                Expected(")");
            }

            return;
        }

        if (CurrentEquals("EXCEPTION-OBJECT"))
        {
            Expected("EXCEPTION-OBJECT");
            if (!HasFlag(allowedTypes, IdentifierType.ExceptionObject))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected EXCEPTION-OBJECT identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This EXCEPTION-OBJECT should not be here.
                    """)
                .WithNote("""
                    EXCEPTION-OBJECT must not be used as receiving operand
                    """)
                .CloseError();
            }

            return;
        }
        
        if (CurrentEquals("SELF"))
        {
            Expected("SELF");
            if (!HasFlag(allowedTypes, IdentifierType.Self))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected SELF identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This SELF should not be here.
                    """)
                .WithNote("""
                    SELF must not be used as receiving operand
                    """)
                .CloseError();
            }

            return;
        }
        
        if (CurrentEquals("NULL"))
        {
            Expected("NULL");
            if (!HasFlag(allowedTypes, IdentifierType.NullAddress) && !HasFlag(allowedTypes, IdentifierType.NullObject))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected NULL identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This NULL reference should not be here.
                    """)
                .WithNote("""
                    NULL must not be used as receiving operand
                    """)
                .CloseError();
            }

            return;
        }
        
        if (CurrentEquals("ADDRESS") && !LookaheadEquals(1, "PROGRAM", "FUNCTION") && !LookaheadEquals(2, "PROGRAM", "FUNCTION"))
        {
            Expected("ADDRESS");
            Optional("OF");
            if (!HasFlag(allowedTypes, IdentifierType.DataAddress))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected ADDRESS OF identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This ADDRESS OF reference should not be here.
                    """)
                .WithNote("""
                    ADDRESS OF must not be used as receiving operand
                    """)
                .CloseError();
            }

            Continue();
            return;
        }
        
        if (CurrentEquals("ADDRESS") && LookaheadEquals(1, "FUNCTION") || LookaheadEquals(2, "FUNCTION"))
        {
            Expected("ADDRESS");
            Optional("OF");
            Expected("FUNCTION");
            if (!HasFlag(allowedTypes, IdentifierType.FunctionAddress))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected ADDRESS OF FUNCTION identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This ADDRESS OF FUNCTION should not be here.
                    """)
                .WithNote("""
                    ADDRESS OF FUNCTION must not be used as receiving operand
                    """)
                .CloseError();
            }

            if (CurrentEquals(TokenType.Identifier))
            {
                Continue();
            }
            else
            {
                Literals.String();
            }

            return;
        }
        
        if (CurrentEquals("ADDRESS") && LookaheadEquals(1, "PROGRAM") || LookaheadEquals(2, "PROGRAM"))
        {
            Expected("ADDRESS");
            Optional("OF");
            Expected("PROGRAM");
            if (!HasFlag(allowedTypes, IdentifierType.ProgramAddress))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected ADDRESS OF PROGRAM identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This ADDRESS OF PROGRAM should not be here.
                    """)
                .WithNote("""
                    ADDRESS OF PROGRAM must not be used as receiving operand
                    """)
                .CloseError();
            }

            if (CurrentEquals(TokenType.Identifier))
            {
                Continue();
            }
            else
            {
                Literals.String();
            }

            return;
        }
        
        if (CurrentEquals("LINAGE-COUNTER"))
        {
            Expected("LINAGE-COUNTER");
            Choice("IN", "OF");
            if (!HasFlag(allowedTypes, IdentifierType.LinageCounter))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected LINAGE-COUNTER identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This LINAGE-COUNTER should not be here.
                    """)
                .WithNote("""
                    LINAGE-COUNTER must not be used as receiving operand
                    """)
                .CloseError();
            }

            Continue();
            return;
        }
        
        if (CurrentEquals("PAGE-COUNTER", "LINE-COUNTER"))
        {
            var token = Current();

            Choice("PAGE-COUNTER", "LINE-COUNTER");
            Choice("IN", "OF");

            if (!HasFlag(allowedTypes, IdentifierType.ReportCounter))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, $"""
                    Unexpected {token.Value} identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This {token.Value} should not be here.
                    """)
                .WithNote($"""
                    {token.Value} must not be used as receiving operand
                    """)
                .CloseError();
            }

            Continue();
            return;
        }

        if (LookaheadEquals(1, "AS"))
        {
            // var isFactory = false;
            // var isStronglyTyped = false;

            // Need to implement identifier resolution first
            // To parse the rest of this identifier correctly
            // and to add extra compile time checks
        
            Continue();
            Expected("AS");

            if (!HasFlag(allowedTypes, IdentifierType.ObjectView))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected Object View identifier.
                    """)
                .WithSourceLine(Current(), $"""
                    This Object View should not be here.
                    """)
                .WithNote("""
                    Object View must not be used as receiving operand
                    """)
                .CloseError();
            }

            if (CurrentEquals("UNIVERSAL"))
            {
                Expected("UNIVERSAL");
                return;
            }

            if (CurrentEquals("Factory"))
            {
                Expected("FACTORY");
                Optional("OF");
                // isFactory = true;
            }

            Continue();

            if (CurrentEquals("ONLY"))
            {
                Expected("ONLY");
                // isStronglyTyped = true
            }

            return;
        }

        if (LookaheadEquals(1, "::"))
        {


            if (!HasFlag(allowedTypes, IdentifierType.MethodInvocation))
            {
                ErrorHandler
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 15, """
                    Unexpected inline method call.
                    """)
                .WithSourceLine(Current(), $"""
                    This method call should not be here.
                    """)
                .WithNote("""
                    Inline method calls must not be used as receiving operand
                    """)
                .CloseError();
            }

            // TODO: Replace Continue with an identifier check for the method name
            Continue();
            Expected("::");
            Literals.String();

            if (CurrentEquals("("))
            {
                // TODO:
                // This needs a check from the symbol table to verify the number and type
                // of the function's parameters.
                Expected("(");
                while (!CurrentEquals(")")) Continue();
                Expected(")");
            }

            return;
        }        

        Continue();

        if (CurrentEquals("("))
        {
            // TODO:
            // This needs a check from the symbol table 
            // to verify the identifier type.
            Expected("(");
            while (!CurrentEquals(")")) Continue();
            Expected(")");
        }
    }

    public static bool Identifier(Token identifierToken, bool useDefaultError = true)
    {
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 0, """
                Unexpected end of file.
                """)
            .WithSourceLine(Lookahead(-1), $"""
                Expected an identifier after this token.
                """)
            .CloseError();

            // Error has already been handled above
            return true;
        }

        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 1, """
                Unexpected token type.
                """)
            .WithSourceLine(Current(), $"""
                Expected a user-defined word (an identifier).
                """)
            .CloseError();

            Continue();

            // Error has already been handled above
            return true;
        }

        var isExpectedToken = CurrentEquals(identifierToken.Value);

        if (!isExpectedToken && useDefaultError)
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 2, """
                Unexpected user-defined name.
                """)
            .WithSourceLine(Current(), $"""
                Expected the following identifier: {identifierToken.Value}.
                """)
            .CloseError();

            // Error has already been handled above
            // Handled using a default error message
            return true;
        }

        if (!isExpectedToken && !useDefaultError)
        {
            // Error has not been handled
            // Expected to be handled by the consumer
            // of this method using an if statement
            return false;
        }
        
        Continue();

        return true;
    }
}
