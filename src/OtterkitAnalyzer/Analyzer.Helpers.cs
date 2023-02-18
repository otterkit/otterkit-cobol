using System.Diagnostics;
using System.Text;

namespace Otterkit;

public static partial class Analyzer
{
    // Analyzer Helper methods.
    // These are the main methods used to interact with and iterate through the List of Tokens.
    // (And other helpers that it easier to iterate through the List)
    // All other methods inside of the analyzer depend on these to parse through the tokens.

    private static void AnchorPoint(params string[] anchors)
    {
        ErrorHandler.Parser.AttemptRecovery(anchors);

        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                Continue();
                return;
            }

            if (CurrentEquals(anchors))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                return;
            }

            Continue();
        }
    }

    private static void AnchorPoint(TokenContext anchor)
    {
        ErrorHandler.Parser.AttemptRecovery(anchor);

        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                Continue();
                return;
            }

            if (CurrentEquals(anchor))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                return;
            }

            Continue();
        }
    }

    private static void CombinedAnchorPoint(TokenContext anchor, params string[] anchors)
    {
        ErrorHandler.Parser.AttemptRecovery(anchors);

        while (!CurrentEquals(TokenType.EOF))
        {
            if (CurrentEquals("."))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                Continue();
                return;
            }

            if (CurrentEquals(anchors) || CurrentEquals(anchor))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Recovery, """
                Parser recovered at the following anchor point: 
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current(), ConsoleColor.Blue);
                return;
            }

            Continue();
        }
    }


    /// <summary>
    /// Token <c>Lookahead</c>: This method returns a Token from an index of Current Index + the amount parameter
    /// <para>When passed a positive amount it will act as a lookahead method, and when passed a negative amount it will act as a lookbehind method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static Token Lookahead(int amount)
    {
        if (Index + amount >= TokenList.Count) return TokenList[^1];

        return Index + amount < 0 ? TokenList[0] : TokenList[Index + amount];
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c> string[]: This method returns true or false depending on if the Token value from an index of Current Index + the first parameter is equal to the values of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static bool LookaheadEquals(int lookahead, params string[] valuesToCompare)
    {
        foreach (var value in valuesToCompare)
        {
            if (Lookahead(lookahead).value.Equals(value, StringComparison.OrdinalIgnoreCase)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c> TokenType[]: This method returns true or false depending on if the Token type from an index of Current Index + the first parameter is equal to the types of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static bool LookaheadEquals(int lookahead, params TokenType[] tokenTypesToCompare)
    {
        foreach (var type in tokenTypesToCompare)
        {
            if (Lookahead(lookahead).type.Equals(type)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c> TokenScope[]: This method returns true or false depending on if the Token scope from an index of Current Index + the first parameter is equal to the scopes of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static bool LookaheadEquals(int lookahead, params TokenScope[] tokenScopesToCompare)
    {
        foreach (var scope in tokenScopesToCompare)
        {
            if (Lookahead(lookahead).scope.Equals(scope)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>LookaheadEquals</c> TokenContext[]: This method returns true or false depending on if the Token context from an index of Current Index + the first parameter is equal to the contexts of the second paramenter
    /// <para>When passed a positive amount it will act as a lookahead comparison method, and when passed a negative amount it will act as a lookbehind comparison method</para>
    /// <para>Technically this method allows for infinite lookahead and lookbehind comparisons, as long as the Index + amount is not bigger than the
    /// number of items on the list of Tokens or smaller than 0.</para>
    /// </summary>
    private static bool LookaheadEquals(int lookahead, params TokenContext[] tokenContextsToCompare)
    {
        foreach (var context in tokenContextsToCompare)
        {
            if (Lookahead(lookahead).context.Equals(context)) return true;
        }

        return false;
    }

    /// <summary>
    /// Token <c>Current</c>: This method returns the current token from the current Index.
    /// <para>The returned token in encapsulated inside of the Token type, which holds not only the value of the token but also some additional context
    /// that might be required by the parser in certain situations</para>
    /// </summary>
    private static Token Current()
    {
        return TokenList[Index];
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> string[]: This method returns true or false depending on if the current token from the current Index has the same value as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    private static bool CurrentEquals(params string[] valuesToCompare)
    {
        foreach (var value in valuesToCompare)
        {
            if (Current().value.Equals(value, StringComparison.OrdinalIgnoreCase)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> TokenType[]: This method returns true or false depending on if the current token from the current Index has the same type as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    private static bool CurrentEquals(params TokenType[] tokenTypesToCompare)
    {
        foreach (var type in tokenTypesToCompare)
        {
            if (Current().type.Equals(type)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> TokenScope[]: This method returns true or false depending on if the current token from the current Index has the same scope as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    private static bool CurrentEquals(params TokenScope[] tokenScopesToCompare)
    {
        foreach (var scope in tokenScopesToCompare)
        {
            if (Current().scope.Equals(scope)) return true;
        }

        return false;
    }

    /// <summary>
    /// Boolean <c>CurrentEquals</c> TokenContext[]: This method returns true or false depending on if the current token from the current Index has the same context as the parameter.
    /// <para>This helper method is an alternative to the <c>"Current().value.Equals()"</c> syntax, which could become verbose and harder to read when it's used frequently</para>
    /// </summary>
    private static bool CurrentEquals(params TokenContext[] tokenContextsToCompare)
    {
        foreach (var context in tokenContextsToCompare)
        {
            if (Current().context.Equals(context)) return true;
        }

        return false;
    }

    /// <summary>
    /// Void <c>Continue</c>: This method adds +1 to the current index, moving the index to the next token.
    /// <para>This method will be called in the Expected(), Optional(), Choice(), Identifier(), Number() and String() methods,
    /// so there's no need to call Continue after calling those methods.</para>
    /// </summary>
    private static void Continue(int amount = 1)
    {
        if (CurrentEquals(TokenType.EOF) && Index >= TokenList.Count - 1) return;

        Index += amount;
    }

    /// <summary>
    /// Void <c>Choice</c>: This method checks if the current token matches one of the values passed in its parameters.
    /// <para>If the current token matches one the values, it moves to the next token,
    /// if the current token doesn't match any of the values it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Choice(params string[] choices)
    {
        Token token = Current();
        foreach (string choice in choices)
        {
            if (CurrentEquals(choice))
            {
                Continue();
                return;
            }
        }

        ErrorHandler.Parser.Report(FileName, token, ErrorType.Choice, choices);
        ErrorHandler.Parser.PrettyError(FileName, token);
        Continue();
    }

    /// <summary>
    /// Void <c>Optional</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the value, it moves to the next token,
    /// if the current token doesn't match the value it ignores the token and returns without moving to the next token</para>
    /// </summary>
    private static void Optional(string optional)
    {
        if (CurrentEquals(optional)) Continue();
    }

    /// <summary>
    /// Void <c>Optional</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the type, it moves to the next token,
    /// if the current token doesn't match the type it ignores the token and returns without moving to the next token</para>
    /// </summary>
    private static void Optional(TokenType optional)
    {
        if (CurrentEquals(optional)) Continue();
    }

    /// <summary>
    /// Void <c>Expected</c>: This method checks if the current token is equal to it's first parameter.
    /// <para>If the current token matches the value, it moves to the next token,
    /// if the current token doesn't match the value it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Expected(string expected, string custom = "default", int position = 0, params string[] wordAnchors)
    {
        var errorMessage = expected;
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected {expected} instead.
            """);

            return;
        }

        if (!CurrentEquals(expected))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);

            if (wordAnchors.Length != 0) AnchorPoint(wordAnchors);

            if (wordAnchors.Length == 0) Continue();
        }
        else
        {
            Continue();
        }
    }

    private static void Expected(string expected, string custom = "default", int position = 0, TokenContext? typeAnchor = null)
    {
        var errorMessage = expected;
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected {expected} instead.
            """);

            return;
        }

        if (!CurrentEquals(expected))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);

            if (typeAnchor is not null) AnchorPoint((TokenContext)typeAnchor);

            if (typeAnchor is null) Continue();
        }
        else
        {
            Continue();
        }
    }

    /// <summary>
    /// Void <c>Identifier</c>: This method checks if the current token is an identifier.
    /// <para>If the current token's type is TokenType.Identifier, it moves to the next token,
    /// if the current token's type is TokenType.Identifier it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Identifier(params UsageType[] allowedUsage)
    {
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected identifier instead.
            """);

            return;
        }

        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, """
            a user-defined name or word (an identifier)
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
            Continue();
            return;
        }

        string dataItemHash = $"{SourceId.Peek()}#{Current().value}";

        if (CurrentSection is CurrentScope.ProcedureDivision)
        {
            if (!SymbolTable.SymbolExists(dataItemHash))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                The name "{Current().value}" does not exist in the context of the current source unit
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        if (allowedUsage.Length >= 1)
        {
            bool hasAllowedUsage = false;

            foreach (var usage in allowedUsage)
            {
                if (SymbolTable.GetDataItem(dataItemHash).UsageType == usage) 
                    hasAllowedUsage = true;
            }

            if (!hasAllowedUsage)
            {
                StringBuilder errorBuilder = new();
                string separator = string.Empty;

                foreach (var usage in allowedUsage)
                {
                    errorBuilder.Append(separator);
                    errorBuilder.Append(usage.Display());
                    separator = ",";
                }

                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                Expected a data item defined with the following: {errorBuilder} USAGE clauses or PICTURE types
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        Continue();
    }

    private static void Identifier(out bool checkFirstUsage, params UsageType[] allowedUsage)
    {
        checkFirstUsage = true;

        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected identifier instead.
            """);

            checkFirstUsage =  false;
            return;
        }

        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, """
            a user-defined name or word (an identifier)
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
            Continue();

            checkFirstUsage =  false;
            return;
        }

        string dataItemHash = $"{SourceId.Peek()}#{Current().value}";

        if (CurrentSection is CurrentScope.ProcedureDivision)
        {
            if (!SymbolTable.SymbolExists(dataItemHash))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                The name "{Current().value}" does not exist in the context of the current source unit
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        if (allowedUsage.Length >= 1)
        {
            bool hasAllowedUsage = false;
            DataItemInfo dataItem = SymbolTable.GetDataItem(dataItemHash);
            foreach (var usage in allowedUsage)
            {
                if (dataItem.UsageType == usage) 
                    hasAllowedUsage = true;

                if (usage != allowedUsage[0])
                    checkFirstUsage = false;
            }

            if (!hasAllowedUsage)
            {
                StringBuilder errorBuilder = new();
                string separator = string.Empty;

                foreach (var usage in allowedUsage)
                {
                    errorBuilder.Append(separator);
                    errorBuilder.Append(usage.Display());
                    separator = ",";
                }

                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
                Expected a data item defined with the following: {errorBuilder} USAGE clauses or PICTURE types
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        Continue();
    }

    private static void Identifier(IdentifierType allowedTypes = IdentifierType.None)
    {
        // TODO:
        // All Identifiers here need a check from the symbol table.
        // The symbol table itself needs to be refactored to accommodate this.
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected identifier instead.
            """);

            return;
        }

        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, """
            a user-defined name or word (an identifier)
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());

            Continue();
            return;
        }

        static bool HasFlag(Enum currentFlags, Enum flag)
        {
            return currentFlags.HasFlag(flag);
        }

        if (CurrentEquals("FUNCTION"))
        {
            Expected("FUNCTION");
            if (!HasFlag(allowedTypes, IdentifierType.Function))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unexpected FUNCTION call. 
                NOTE: Function calls cannot be specified as a receiving operand
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
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
        else if (CurrentEquals("EXCEPTION-OBJECT"))
        {
            Expected("EXCEPTION-OBJECT");
            if (!HasFlag(allowedTypes, IdentifierType.ExceptionObject))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unexpected reference to EXCEPTION-OBJECT. 
                NOTE: EXCEPTION-OBJECT cannot be specified as a receiving operand
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            return;
        }
        else if (CurrentEquals("SELF"))
        {
            Expected("SELF");
            if (!HasFlag(allowedTypes, IdentifierType.Self))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unexpected reference to SELF.
                NOTE: SELF cannot be specified as a receiving operand
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            return;
        }
        else if (CurrentEquals("NULL"))
        {
            Expected("NULL");
            if (!HasFlag(allowedTypes, IdentifierType.NullAddress) && !HasFlag(allowedTypes, IdentifierType.NullObject))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unexpected reference to a NULL address or NULL object.
                NOTE: NULL cannot be specified as a receiving operand
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            return;
        }
        else if (CurrentEquals("ADDRESS") && !LookaheadEquals(1, "PROGRAM", "FUNCTION") && !LookaheadEquals(2, "PROGRAM", "FUNCTION"))
        {
            Expected("ADDRESS");
            Optional("OF");
            if (!HasFlag(allowedTypes, IdentifierType.DataAddress))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unexpected reference to the address of a data item. 
                NOTE: Data item addresses cannot be specified as a receiving operand
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            Continue();
            return;
        }
        else if (CurrentEquals("ADDRESS") && LookaheadEquals(1, "FUNCTION") && !LookaheadEquals(2, "FUNCTION"))
        {
            Expected("ADDRESS");
            Optional("OF");
            Expected("FUNCTION");
            if (!HasFlag(allowedTypes, IdentifierType.FunctionAddress))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unexpected reference to the address of a function. 
                NOTE: Function addresses cannot be specified as a receiving operand
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            if (CurrentEquals(TokenType.Identifier))
            {
                Continue();
            }
            else
            {
                String();
            }

            return;
        }
        else if (CurrentEquals("ADDRESS") && LookaheadEquals(1, "PROGRAM") && !LookaheadEquals(2, "PROGRAM"))
        {
            Expected("ADDRESS");
            Optional("OF");
            Expected("PROGRAM");
            if (!HasFlag(allowedTypes, IdentifierType.ProgramAddress))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unexpected reference to the address of a program. 
                NOTE: Program addresses cannot be specified as a receiving operand
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            if (CurrentEquals(TokenType.Identifier))
            {
                Continue();
            }
            else
            {
                String();
            }

            return;
        }
        else if (CurrentEquals("LINAGE-COUNTER"))
        {
            Expected("LINAGE-COUNTER");
            Choice("IN", "OF");
            if (!HasFlag(allowedTypes, IdentifierType.LinageCounter))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unexpected reference to a LINAGE-COUNTER. 
                NOTE: LINAGE-COUNTER cannot be specified as a receiving operand
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            Continue();
            return;
        }
        else if (CurrentEquals("PAGE-COUNTER", "LINE-COUNTER"))
        {
            Choice("PAGE-COUNTER", "LINE-COUNTER");
            Choice("IN", "OF");
            if (!HasFlag(allowedTypes, IdentifierType.ReportCounter))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Unexpected reference to a report counter. 
                NOTE: Report counters cannot be specified as a receiving operand
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            Continue();
            return;
        }

        Continue();
    }

    private static void Identifier(string identifierString)
    {
        if (CurrentEquals(TokenType.EOF))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Unexpected End Of File. Expected identifier instead.
            """);

            return;
        }

        if (!CurrentEquals(TokenType.Identifier))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.Expected, """
            a user-defined name or word (an identifier)
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
            Continue();
            return;
        }

        if (!CurrentEquals(identifierString))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Expected a user-defined name (an identifier) with value: "{identifierString}"
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
        }

        Continue();
    }

    /// <summary>
    /// Void <c>Number</c>: This method checks if the current token is a Number.
    /// <para>If the current token's type is TokenType.Numeric, it moves to the next token,
    /// if the current token's type is TokenType.Numeric it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Number(string custom = "default", int position = 0)
    {
        var errorMessage = "Numeric literal";
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(TokenType.Numeric))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
        }
        else
        {
            Continue();
        }
    }

    /// <summary>
    /// Void <c>String</c>: This method checks if the current token is a National, Alphanumeric, Alphabetic or Boolean.
    /// <para>If the current token's type is TokenType.String, it moves to the next token,
    /// if the current token's type is TokenType.String it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void String(string custom = "default", int position = 0)
    {
        var errorMessage = "String literal";
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(
            TokenType.String, 
            TokenType.HexString, 
            TokenType.Boolean, 
            TokenType.HexBoolean, 
            TokenType.National, 
            TokenType.HexNational
        ))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
        }
        else
        {
            Continue();
        }
    }

    /// <summary>
    /// Void <c>FigurativeLiteral</c>: This method checks if the current token is a Figurative Literal.
    /// <para>If the current token's type is TokenType.FigurativeLiteral, it moves to the next token,
    /// if the current token's type is TokenType.FigurativeLiteral it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void FigurativeLiteral(string custom = "default", int position = 0)
    {
        var errorMessage = "Numeric literal";
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(TokenType.FigurativeLiteral))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
        }
        else
        {
            Continue();
        }
    }

    /// <summary>
    /// Void <c>Symbol</c>: This method checks if the current token is a valid COBOl Symbol.
    /// <para>If the current token's type is TokenType.Symbol, it moves to the next token,
    /// if the current token's type is TokenType.Symbol it calls the ErrorHandler to report a parsing error</para>
    /// </summary>
    private static void Symbol(string custom = "default", int position = 0)
    {
        var errorMessage = "a COBOL symbol";
        var errorType = ErrorType.Expected;
        if (!custom.Equals("default"))
        {
            errorMessage = custom;
            errorType = ErrorType.General;
        }

        if (!CurrentEquals(TokenType.Symbol))
        {
            var lookahead = Lookahead(position);

            ErrorHandler.Parser.Report(FileName, lookahead, errorType, errorMessage);
            ErrorHandler.Parser.PrettyError(FileName, lookahead);
            Continue();
        }
        else
        {
            Continue();
        }
    }

}