using static Otterkit.Types.TokenHandling;
using static Otterkit.Types.CompilerContext;
using Otterkit.Types;
using System.Diagnostics;

namespace Otterkit.Analyzers;

public static partial class References
{
    private static Stack<Token> Qualification = new();

    public static bool HasFlag(Enum type, Enum flag)
        => type.HasFlag(flag);

    private static bool CheckParent(Token entry, Token parent)
    {
        var entries = ActiveData.EntriesList(entry);

        foreach (var item in entries)
        {
            if (!item.Parent.Exists)
            {
                continue;
            }

            var parentEntry = item.Parent.Unwrap();

            if (!parentEntry.Identifier.Exists)
            {
                continue;
            }

            var parentToken = parentEntry.Identifier.Unwrap();

            if (parentToken.Value == parent.Value)
            {
                Qualification.Push(parentToken);

                return true;
            }
        }

        return false;
    }

    private static Token FindSubordinate(Token qualified, Token subordinate)
    {
        var entries = ActiveData.EntriesList(subordinate);

        foreach (var item in entries)
        {
            if (!item.Parent.Exists)
            {
                continue;
            }

            var parentEntry = item.Parent.Unwrap();

            if (!parentEntry.Identifier.Exists)
            {
                continue;
            }

            var parentToken = parentEntry.Identifier.Unwrap();

            if (parentToken.Line == qualified.Line && parentToken.Column == qualified.Column)
            {
                return item.Identifier.Unwrap();
            }
        }

        throw new UnreachableException($"Unable to find the subordinate token qualified by {qualified}.");
    }

    private static Token FindFullyQualified()
    {
        var qualified = Qualification.Pop();

        foreach (var subordinate in Qualification)
        {
            qualified = FindSubordinate(qualified, subordinate);
        }

        return qualified;
    }

    public static Unique<Token> LocalName(bool shouldResolve = true)
    {
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 0, """
                Unexpected end of file.
                """)
            .WithSourceLine(Peek(-1), $"""
                Expected an identifier after this token.
                """)
            .CloseError();

            return null;
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

            return null;
        }

        var nameToken = Current();

        if (!IsResolutionPass || !shouldResolve)
        {
            Continue();
            return null;
        }

        var (exists, isUnique) = ActiveData.HasUniqueEntry(nameToken);

        if (!exists)
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 15, """
                Reference to undefined identifier.
                """)
            .WithSourceLine(Current(), $"""
                Name does not exist in the current context.
                """)
            .CloseError();

            Continue();

            return null;
        }

        Continue();

        return (nameToken, isUnique);
    }

    public static Unique<Token> Qualified(TokenContext anchorPoint = TokenContext.IsStatement)
    {
        if (!IsResolutionPass)
        {
            LocalName();

            while (CurrentEquals("IN OF"))
            {
                Choice("IN OF");

                LocalName();
            }

            return null;
        }

        var name = LocalName();

        if (!name.Exists)
        {
            AnchorPoint(anchorPoint);

            return null;
        }

        var nameToken = name.Unwrap();

        if (!name.IsUnique && !CurrentEquals("IN OF"))
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 15, """
                Reference to non-unique identifier.
                """)
            .WithSourceLine(nameToken, $"""
                Name requires qualification.
                """)
            .CloseError();

            return null;
        }

        Qualification.Push(nameToken);

        while (CurrentEquals("IN OF"))
        {
            Choice("IN OF");

            var parent = LocalName();

            if (!parent.Exists) continue;

            var parentToken = parent.Unwrap();

            var isParent = CheckParent(nameToken, parentToken);

            if (!isParent)
            {
                ErrorHandler
                .Build(ErrorType.Resolution, ConsoleColor.Red, 15, """
                    Reference to incorrect superordinate.
                    """)
                .WithSourceLine(parentToken, $"""
                    Name does not have a {nameToken.Value} field.
                    """)
                .CloseError();
            }

            if (!parent.IsUnique && !CurrentEquals("IN OF"))
            {
                ErrorHandler
                .Build(ErrorType.Resolution, ConsoleColor.Red, 15, """
                    Reference to non-unique identifier.
                    """)
                .WithSourceLine(parentToken, $"""
                    Name requires qualification.
                    """)
                .CloseError();

                return null;
            }

            nameToken = parentToken;
        }

        var fullyQualified = FindFullyQualified();

        Qualification.Clear();

        return fullyQualified;
    }

    public static DataEntry FetchDataEntry(Token qualifiedToken)
    {
        var entries = ActiveData.EntriesList(qualifiedToken);

        foreach (var entry in entries)
        {
            if (entry.Identifier == qualifiedToken)
            {
                return entry;
            }
        }

        throw new ArgumentException("Token was not qualified. Could not resolve data item", nameof(qualifiedToken));
    }

    public static void Identifier(Identifiers allowed = Identifiers.None)
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
            .WithSourceLine(Peek(-1), $"""
                Expected an identifier after this token.
                """)
            .CloseError();

            return;
        }

        if (CurrentEquals("FUNCTION"))
        {
            Expected("FUNCTION");
            if (!HasFlag(allowed, Identifiers.Function))
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
            if (!HasFlag(allowed, Identifiers.ExceptionObject))
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
            if (!HasFlag(allowed, Identifiers.Self))
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
            if (!HasFlag(allowed, Identifiers.NullAddress) && !HasFlag(allowed, Identifiers.NullObject))
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

        if (CurrentEquals("ADDRESS") && !PeekEquals(1, "PROGRAM FUNCTION") && !PeekEquals(2, "PROGRAM FUNCTION"))
        {
            Expected("ADDRESS");
            Optional("OF");
            if (!HasFlag(allowed, Identifiers.DataAddress))
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

        if (CurrentEquals("ADDRESS") && PeekEquals(1, "FUNCTION") || PeekEquals(2, "FUNCTION"))
        {
            Expected("ADDRESS");
            Optional("OF");
            Expected("FUNCTION");
            if (!HasFlag(allowed, Identifiers.FunctionAddress))
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

        if (CurrentEquals("ADDRESS") && PeekEquals(1, "PROGRAM") || PeekEquals(2, "PROGRAM"))
        {
            Expected("ADDRESS");
            Optional("OF");
            Expected("PROGRAM");
            if (!HasFlag(allowed, Identifiers.ProgramAddress))
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
            Choice("IN OF");
            if (!HasFlag(allowed, Identifiers.LinageCounter))
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

        if (CurrentEquals("PAGE-COUNTER LINE-COUNTER"))
        {
            var token = Current();

            Choice("PAGE-COUNTER LINE-COUNTER");
            Choice("IN OF");

            if (!HasFlag(allowed, Identifiers.ReportCounter))
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

        if (PeekEquals(1, "AS"))
        {
            // var isFactory = false;
            // var isStronglyTyped = false;

            // Need to implement identifier resolution first
            // To parse the rest of this identifier correctly
            // and to add extra compile time checks

            Continue();
            Expected("AS");

            if (!HasFlag(allowed, Identifiers.ObjectView))
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

        if (PeekEquals(1, "::"))
        {


            if (!HasFlag(allowed, Identifiers.MethodInvocation))
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
            .WithSourceLine(Peek(-1), $"""
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

    public static Option<Token> GlobalName(bool shouldExist, bool shouldResolve = true)
    {
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 0, """
                Unexpected end of file.
                """)
            .WithSourceLine(Peek(-1), $"""
                Expected an identifier after this token.
                """)
            .CloseError();

            return null;
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

            return null;
        }

        var nameToken = Current();

        if (!IsResolutionPass || !shouldResolve)
        {
            Continue();
            return null;
        }

        var exists = ActiveNames.Exists(nameToken);

        if (!exists && shouldExist)
        {
            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 15, """
                Reference to undefined identifier.
                """)
            .WithSourceLine(Current(), $"""
                Name does not exist in the current context.
                """)
            .CloseError();

            Continue();

            return null;
        }

        if (exists && !shouldExist)
        {
            var original = ActiveNames.Fetch(nameToken);

            var token = original.Identifier;

            ErrorHandler
            .Build(ErrorType.Resolution, ConsoleColor.Red, 15, """
                Duplicate global name definition.
                """)
            .WithSourceLine(Current(), $"""
                Already defined in this codebase.
                """)
            .WithSourceNote(token)
            .WithNote("""
                The original name was defined here.
                """)
            .CloseError();

            Continue();

            return null;
        }

        Continue();

        return nameToken;
    }
}
