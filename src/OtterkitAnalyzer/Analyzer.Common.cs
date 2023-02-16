using System.Diagnostics;
using System.Text;

namespace Otterkit;

public struct SetLcValues
{
    public bool LC_ALL;
    public bool LC_COLLATE;
    public bool LC_CTYPE;
    public bool LC_MESSAGES;
    public bool LC_MONETARY;
    public bool LC_NUMERIC;
    public bool LC_TIME;
}

public static partial class Analyzer
{
    // The following methods are responsible for parsing some commonly repeated pieces of COBOL statements.
    // The ON SIZE ERROR, ON EXCEPTION, INVALID KEY, AT END, and the RETRY phrase are examples of pieces of COBOL syntax
    // that appear on multiple statements. Reusing the same code in those cases keeps things much more modular and easier to maintain.
    //
    // The Arithmetic() and Condition() methods are responsible for parsing expressions and verifying if those expressions were
    // written correctly. This is using a combination of the Shunting Yard algorithm, and some methods to verify if the 
    // parentheses are balanced and if it can be evaluated correctly.
    
    public static void TimesPhrase()
    {
        if (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }
        else
        {
            Number();
        }

        Expected("TIMES");
    }

    public static void UntilPhrase()
    {
        Expected("UNTIL");
        if (CurrentEquals("EXIT"))
        {
            Expected("EXIT");
        }
        else
        {
            Condition();
        }
    }

    public static void VaryingPhrase()
    {
        Expected("VARYING");
        Identifier();
        Expected("FROM");
        if (CurrentEquals(TokenType.Numeric))
        {
            Number();
        }
        else
        {
            Identifier();
        }

        if (CurrentEquals("BY"))
        {
            Expected("BY");
            if (CurrentEquals(TokenType.Numeric))
            {
                Number();
            }
            else
            {
                Identifier();
            }
        }

        Expected("UNTIL");
        Condition("AFTER");

        while (CurrentEquals("AFTER"))
        {
            Expected("AFTER");
            Identifier();
            Expected("FROM");
            if (CurrentEquals(TokenType.Numeric))
            {
                Number();
            }
            else
            {
                Identifier();
            }

            if (CurrentEquals("BY"))
            {
                Expected("BY");
                if (CurrentEquals(TokenType.Numeric))
                {
                    Number();
                }
                else
                {
                    Identifier();
                }
            }

            Expected("UNTIL");
            Condition("AFTER");
        }
    }

    public static void WithTest()
    {
        if (CurrentEquals("WITH", "TEST"))
        {
            Optional("WITH");
            Expected("TEST");
            Choice("BEFORE", "AFTER");
        }
    }

    public static void RetryPhrase()
    {
        var hasFor = false;

        Expected("RETRY");
        if (CurrentEquals("FOREVER"))
        {
            Expected("FOREVER");
            return;
        }

        if (CurrentEquals("FOR"))
        {
            Optional("FOR");
            hasFor = true;
        }

        Arithmetic("SECONDS", "TIMES");
        if (CurrentEquals("SECONDS") || hasFor)
        {
            Expected("SECONDS");
        }
        else
        {
            Expected("TIMES");
        }
    }

    public static void TallyingPhrase()
    {
        if (!CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "FOR"))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
            The tallying phrase must start with a data item identifier, which must be followed by the FOR keyword
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
        }

        while (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "FOR"))
        {
            Identifier();
            Expected("FOR");

            if (!CurrentEquals("CHARACTERS", "ALL", "LEADING"))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The tallying phrase must contain at least one of the following clauses: CHARACTERS, ALL or LEADING
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            while (CurrentEquals("CHARACTERS", "ALL", "LEADING"))
            {
                if (CurrentEquals("CHARACTERS"))
                {
                    Expected("CHARACTERS");
                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }
                }
                else if (CurrentEquals("ALL"))
                {
                    Expected("ALL");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    while (CurrentEquals(TokenType.Identifier, TokenType.String))
                    {
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }
                    }
                }
                else if (CurrentEquals("LEADING"))
                {
                    Expected("LEADING");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    while (CurrentEquals(TokenType.Identifier, TokenType.String))
                    {
                        if (CurrentEquals(TokenType.Identifier))
                        {
                            Identifier();
                        }
                        else
                        {
                            String();
                        }

                        if (CurrentEquals("AFTER", "BEFORE"))
                        {
                            AfterBeforePhrase();
                        }
                    }
                }
            }
        }
    }

    public static void ReplacingPhrase()
    {
        if (!CurrentEquals("CHARACTERS", "ALL", "LEADING", "FIRST"))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
            The replacing phrase must contain at least one of the following clauses: CHARACTERS, ALL, LEADING or FIRST
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
        }

        while (CurrentEquals("CHARACTERS", "ALL", "LEADING", "FIRST"))
        {
            if (CurrentEquals("CHARACTERS"))
            {
                Expected("CHARACTERS");
                Expected("BY");

                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                if (CurrentEquals("AFTER", "BEFORE"))
                {
                    AfterBeforePhrase();
                }
            }
            else if (CurrentEquals("ALL"))
            {
                Expected("ALL");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                Expected("BY");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                if (CurrentEquals("AFTER", "BEFORE"))
                {
                    AfterBeforePhrase();
                }

                while (CurrentEquals(TokenType.Identifier, TokenType.String))
                {
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    Expected("BY");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }
                }
            }
            else if (CurrentEquals("LEADING"))
            {
                Expected("LEADING");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                Expected("BY");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                if (CurrentEquals("AFTER", "BEFORE"))
                {
                    AfterBeforePhrase();
                }

                if (CurrentEquals("AFTER", "BEFORE"))
                {
                    AfterBeforePhrase();
                }

                while (CurrentEquals(TokenType.Identifier, TokenType.String))
                {
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    Expected("BY");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }
                }
            }
            else if (CurrentEquals("FIRST"))
            {
                Expected("FIRST");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                Expected("BY");
                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }

                if (CurrentEquals("AFTER", "BEFORE"))
                {
                    AfterBeforePhrase();
                }

                if (CurrentEquals("AFTER", "BEFORE"))
                {
                    AfterBeforePhrase();
                }

                while (CurrentEquals(TokenType.Identifier, TokenType.String))
                {
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    Expected("BY");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else
                    {
                        String();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }

                    if (CurrentEquals("AFTER", "BEFORE"))
                    {
                        AfterBeforePhrase();
                    }
                }
            }
        }
    }

    public static void AfterBeforePhrase(bool beforeExists = false, bool afterExists = false)
    {
        if (CurrentEquals("AFTER"))
        {
            if (afterExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                AFTER can only be specified once in this part of the statement. 
                The same applies to BEFORE.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            afterExists = true;
            Expected("AFTER");
            Optional("INITIAL");

            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                String();
            }

            AfterBeforePhrase(beforeExists, afterExists);

        }

        if (CurrentEquals("BEFORE"))
        {
            if (beforeExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                BEFORE can only be specified once in this part of the statement. 
                The same applies to AFTER.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            beforeExists = true;
            Expected("BEFORE");
            Optional("INITIAL");

            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                String();
            }

            AfterBeforePhrase(beforeExists, afterExists);
        }
    }

    public static void InvalidKey(ref bool isConditional, bool invalidKeyExists = false, bool notInvalidKeyExists = false)
    {
        if (CurrentEquals("INVALID"))
        {
            if (invalidKeyExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                INVALID KEY can only be specified once in this statement. 
                The same applies to the NOT INVALID KEY.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            invalidKeyExists = true;
            Expected("INVALID");
            Optional("KEY");
            ParseStatements(true);
            InvalidKey(ref isConditional, invalidKeyExists, notInvalidKeyExists);

        }

        if (CurrentEquals("NOT"))
        {
            if (notInvalidKeyExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                NOT INVALID KEY can only be specified once in this statement. 
                The same applies to the INVALID KEY.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            notInvalidKeyExists = true;
            Expected("NOT");
            Expected("INVALID");
            Optional("KEY");
            ParseStatements(true);
            InvalidKey(ref isConditional, invalidKeyExists, notInvalidKeyExists);
        }
    }

    public static void OnException(ref bool isConditional, bool onExceptionExists = false, bool notOnExceptionExists = false)
    {
        if (CurrentEquals("ON") || CurrentEquals("EXCEPTION"))
        {
            if (onExceptionExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                ON EXCEPTION can only be specified once in this statement. 
                The same applies to the NOT ON EXCEPTION.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            onExceptionExists = true;
            Optional("ON");
            Expected("EXCEPTION");
            ParseStatements(true);
            OnException(ref isConditional, onExceptionExists, notOnExceptionExists);

        }

        if (CurrentEquals("NOT"))
        {
            if (notOnExceptionExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                NOT ON EXCEPTION can only be specified once in this statement. 
                The same applies to the ON EXCEPTION.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            notOnExceptionExists = true;
            Expected("NOT");
            Optional("ON");
            Expected("EXCEPTION");
            ParseStatements(true);
            OnException(ref isConditional, onExceptionExists, notOnExceptionExists);
        }
    }

    public static void RaisingStatus(bool raisingExists = false, bool statusExists = false)
    {
        if (CurrentEquals("RAISING"))
        {
            if (raisingExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                RAISING can only be specified once in this statement. 
                The same applies to the WITH NORMAL/ERROR STATUS.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            Expected("RAISING");
            if (CurrentEquals("EXCEPTION"))
            {
                Expected("EXCEPTION");
                Identifier();
            }
            else if (CurrentEquals("LAST"))
            {
                Expected("LAST");
                Optional("EXCEPTION");
            }
            else
                Identifier();

            raisingExists = true;
            RaisingStatus(raisingExists, statusExists);

        }

        if (CurrentEquals("WITH") || CurrentEquals("NORMAL") || CurrentEquals("ERROR"))
        {
            if (statusExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                WITH NORMAL/ERROR STATUS can only be specified once in this statement. 
                The same applies to the RAISING.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            Optional("WITH");
            Choice("NORMAL", "ERROR");
            Optional("STATUS");
            switch (Current().type)
            {
                case TokenType.Identifier:
                    Identifier();
                    break;
                case TokenType.Numeric:
                    Number();
                    break;
                case TokenType.String:
                    String();
                    break;
            }

            statusExists = true;
            RaisingStatus(raisingExists, statusExists);
        }
    }

    public static void AtEnd(ref bool isConditional, bool atEndExists = false, bool notAtEndExists = false)
    {
        if (CurrentEquals("AT") || CurrentEquals("END"))
        {
            if (atEndExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                AT END can only be specified once in this statement. 
                The same applies to the NOT AT END.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            atEndExists = true;
            Optional("AT");
            Expected("END");
            ParseStatements(true);
            AtEnd(ref isConditional, atEndExists, notAtEndExists);

        }

        if (CurrentEquals("NOT"))
        {
            if (notAtEndExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                NOT AT END can only be specified once in this statement. 
                The same applies to the AT END.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            notAtEndExists = true;
            Expected("NOT");
            Optional("AT");
            Expected("END");
            ParseStatements(true);
            AtEnd(ref isConditional, atEndExists, notAtEndExists);
        }
    }

    public static void SizeError(ref bool isConditional, bool onErrorExists = false, bool notOnErrorExists = false)
    {
        if (CurrentEquals("ON") || CurrentEquals("SIZE"))
        {
            if (onErrorExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                ON SIZE ERROR can only be specified once in this statement. 
                The same applies to NOT ON SIZE ERROR.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            onErrorExists = true;
            Optional("ON");
            Expected("SIZE");
            Expected("ERROR");
            ParseStatements(true);
            SizeError(ref isConditional, onErrorExists, notOnErrorExists);

        }

        if (CurrentEquals("NOT"))
        {
            if (notOnErrorExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                NOT ON SIZE ERROR can only be specified once in this statement. 
                The same applies to ON SIZE ERROR.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            notOnErrorExists = true;
            Expected("NOT");
            Optional("ON");
            Expected("SIZE");
            Expected("ERROR");
            ParseStatements(true);
            SizeError(ref isConditional, onErrorExists, notOnErrorExists);
        }
    }

    public static void OnOverflow(ref bool isConditional, bool onOverflowExists = false, bool notOnOverflowExists = false)
    {
        if (CurrentEquals("ON") || CurrentEquals("OVERFLOW"))
        {
            if (onOverflowExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                ON OVERFLOW can only be specified once in this statement. 
                The same applies to NOT ON OVERFLOW.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            onOverflowExists = true;
            Optional("ON");
            Expected("OVERFLOW");
            ParseStatements(true);
            OnOverflow(ref isConditional, onOverflowExists, notOnOverflowExists);

        }

        if (CurrentEquals("NOT"))
        {
            if (notOnOverflowExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                NOT ON OVERFLOW can only be specified once in this statement. 
                The same applies to ON OVERFLOW.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            notOnOverflowExists = true;
            Expected("NOT");
            Optional("ON");
            Expected("OVERFLOW");
            ParseStatements(true);
            OnOverflow(ref isConditional, onOverflowExists, notOnOverflowExists);
        }
    }

    public static void WriteBeforeAfter(bool beforeExists = false, bool afterExists = false)
    {
        if (CurrentEquals("BEFORE"))
        {
            if (beforeExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                BEFORE can only be specified once in this statement. 
                The same applies to AFTER.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            beforeExists = true;
            Expected("BEFORE");

            WriteBeforeAfter(beforeExists, afterExists);

        }

        if (CurrentEquals("AFTER"))
        {
            if (afterExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                AFTER can only be specified once in this statement. 
                The same applies to BEFORE.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            afterExists = true;
            Expected("AFTER");

            WriteBeforeAfter(beforeExists, afterExists);
        }
    }

    public static void SetLocale(SetLcValues locales = new())
    {
        if (CurrentEquals("LC_ALL"))
        {
            if (locales.LC_ALL)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                LC_ALL can only be specified once in this statement. 
                The same applies to each of the other locale names.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            locales.LC_ALL = true;
            Expected("LC_ALL");

            SetLocale(locales);

        }

        if (CurrentEquals("LC_COLLATE"))
        {
            if (locales.LC_COLLATE)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                LC_COLLATE can only be specified once in this statement. 
                The same applies to each of the other locale names.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            locales.LC_COLLATE = true;
            Expected("LC_COLLATE");

            SetLocale(locales);

        }

        if (CurrentEquals("LC_CTYPE"))
        {
            if (locales.LC_CTYPE)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                LC_CTYPE can only be specified once in this statement. 
                The same applies to each of the other locale names.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            locales.LC_CTYPE = true;
            Expected("LC_CTYPE");

            SetLocale(locales);

        }

        if (CurrentEquals("LC_MESSAGES"))
        {
            if (locales.LC_MESSAGES)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                LC_MESSAGES can only be specified once in this statement. 
                The same applies to each of the other locale names.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            locales.LC_MESSAGES = true;
            Expected("LC_MESSAGES");

            SetLocale(locales);

        }

        if (CurrentEquals("LC_MONETARY"))
        {
            if (locales.LC_MONETARY)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                LC_MONETARY can only be specified once in this statement. 
                The same applies to each of the other locale names.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            locales.LC_MONETARY = true;
            Expected("LC_MONETARY");

            SetLocale(locales);

        }

        if (CurrentEquals("LC_NUMERIC"))
        {
            if (locales.LC_NUMERIC)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                LC_NUMERIC can only be specified once in this statement. 
                The same applies to each of the other locale names.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            locales.LC_NUMERIC = true;
            Expected("LC_NUMERIC");

            SetLocale(locales);

        }

        if (CurrentEquals("LC_TIME"))
        {
            if (locales.LC_TIME)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                LC_TIME can only be specified once in this statement. 
                The same applies to each of the other locale names.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            locales.LC_TIME = true;
            Expected("LC_TIME");

            SetLocale(locales);

        }
    }

    public static void AtEndOfPage(ref bool isConditional, bool atEndOfPageExists = false, bool notAtEndOfPageExists = false)
    {
        if (CurrentEquals("AT", "END-OF-PAGE", "EOP"))
        {
            if (atEndOfPageExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                AT END-OF-PAGE can only be specified once in this statement. 
                The same applies to NOT AT END-OF-PAGE.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            atEndOfPageExists = true;
            Optional("AT");
            Choice("END-OF-PAGE", "EOP");
            ParseStatements(true);
            AtEndOfPage(ref isConditional, atEndOfPageExists, notAtEndOfPageExists);

        }

        if (CurrentEquals("NOT"))
        {
            if (notAtEndOfPageExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                NOT AT END-OF-PAGE can only be specified once in this statement. 
                The same applies to AT END-OF-PAGE.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            isConditional = true;
            notAtEndOfPageExists = true;
            Expected("NOT");
            Optional("AT");
            Choice("END-OF-PAGE", "EOP");
            ParseStatements(true);
            AtEndOfPage(ref isConditional, atEndOfPageExists, notAtEndOfPageExists);
        }
    }

    public static void ForAlphanumericForNational(bool forAlphanumericExists = false, bool forNationalExists = false)
    {
        if (CurrentEquals("FOR") && LookaheadEquals(1, "ALPHANUMERIC") || CurrentEquals("ALPHANUMERIC"))
        {
            if (forAlphanumericExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                FOR ALPHANUMERIC can only be specified once in this statement. 
                The same applies to FOR NATIONAL.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            forAlphanumericExists = true;
            Optional("FOR");
            Expected("ALPHANUMERIC");
            Optional("IS");
            Identifier();

            ForAlphanumericForNational(forAlphanumericExists, forNationalExists);

        }

        if (CurrentEquals("FOR") && LookaheadEquals(1, "NATIONAL") || CurrentEquals("NATIONAL"))
        {
            if (forNationalExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                FOR NATIONAL can only be specified once in this statement. 
                The same applies to FOR ALPHANUMERIC.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            forNationalExists = true;
            Optional("FOR");
            Expected("NATIONAL");
            Optional("IS");
            Identifier();

            ForAlphanumericForNational(forAlphanumericExists, forNationalExists);
        }
    }

    public static void LineColumn(bool lineExists = false, bool columnExists = false)
    {
        if (CurrentEquals("LINE"))
        {
            if (lineExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                LINE NUMBER can only be specified once in this statement. 
                The same applies to the COLUMN NUMBER.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            lineExists = true;
            Expected("LINE");
            Optional("NUMBER");
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                Number();
            }


            LineColumn(lineExists, columnExists);

        }

        if (CurrentEquals("COLUMN", "COL"))
        {
            if (columnExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                COLUMN NUMBER can only be specified once in this statement. 
                The same applies to the LINE NUMBER.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }

            columnExists = true;
            Expected(Current().value);
            Optional("NUMBER");
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                Number();
            }

            LineColumn(lineExists, columnExists);
        }
    }

    public static void Arithmetic(params string[] delimiter)
    {
        static bool IsArithmeticSymbol(Token current)
        {
            return Helpers.ArithmeticPrecedence.ContainsKey(current.value);
        }

        var expression = new List<Token>();

        while (!CurrentEquals(TokenType.ReservedKeyword) && !CurrentEquals(delimiter))
        {
            if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
            {
                expression.Add(Current());
                Continue();
            }

            if (IsArithmeticSymbol(Current()))
            {
                expression.Add(Current());
                Continue();
            }

            if (CurrentEquals(TokenType.Symbol) && !CurrentEquals(".") && !IsArithmeticSymbol(Current()))
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                Invalid symbol in this arithmetic expression. Valid operators are: +, -, *, /, **, ( and )
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
        }

        if (!Helpers.IsBalanced(expression))
        {
            ErrorHandler.Parser.Report(FileName, expression[0], ErrorType.General, """
            This expression is not balanced, one or more parenthesis to not have their matching opening or closing pair, it is an invalid expression
            """);
            ErrorHandler.Parser.PrettyError(FileName, expression[0]);
        }

        var shuntingYard = Helpers.ShuntingYard(expression, Helpers.ArithmeticPrecedence);

        if (!Helpers.EvaluatePostfix(shuntingYard, Helpers.ArithmeticPrecedence, out Token error))
        {
            ErrorHandler.Parser.Report(FileName, error, ErrorType.General, """
            This expression cannot be correctly evaluated. Please make sure that all operators have their matching operands.
            """);
            ErrorHandler.Parser.PrettyError(FileName, error);
        }
    }

    public static void Condition(params string[] delimiter)
    {
        var expression = new List<Token>();

        while (!CurrentEquals(TokenContext.IsStatement) && !CurrentEquals(delimiter))
        {
            if (CurrentEquals("IS") && (Lookahead(1).value is "GREATER" or "LESS" or "EQUAL" or "NOT" || Lookahead(1).type is TokenType.Symbol))
            {
                Continue();
            }
            else if (CurrentEquals("NOT") && (LookaheadEquals(1, ">") || LookaheadEquals(1, "<")))
            {
                var combined = new Token($"NOT {Lookahead(1).value}", TokenType.Symbol, Current().line, Current().column);
                expression.Add(combined);
                Continue(2);
            }
            else if (CurrentEquals("NOT") && (LookaheadEquals(1, "GREATER") || LookaheadEquals(1, "LESS") || LookaheadEquals(1, "EQUAL")))
            {
                if (LookaheadEquals(1, "GREATER"))
                {
                    var combined = new Token($"NOT >", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(combined);
                }

                if (LookaheadEquals(1, "LESS"))
                {
                    var combined = new Token($"NOT <", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(combined);
                }

                if (LookaheadEquals(1, "EQUAL"))
                {
                    var combined = new Token($"<>", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(combined);
                }

                Continue(2);

                if (CurrentEquals("THAN", "TO")) Continue();
            }
            else if (CurrentEquals("GREATER") || CurrentEquals("LESS") || CurrentEquals("EQUAL"))
            {
                if (CurrentEquals("GREATER"))
                {
                    var converted = new Token($">", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(converted);
                }

                if (CurrentEquals("LESS"))
                {
                    var converted = new Token($"<", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(converted);
                }

                if (CurrentEquals("EQUAL"))
                {
                    var converted = new Token($"=", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(converted);
                }

                if (CurrentEquals("GREATER") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
                {
                    if (!LookaheadEquals(1, "THAN")) Continue(2);

                    if (LookaheadEquals(1, "THAN")) Continue(3);

                    var converted = new Token($">=", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(converted);
                }

                if (CurrentEquals("LESS") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
                {
                    if (LookaheadEquals(1, "THAN")) Continue(3);

                    if (!LookaheadEquals(1, "THAN")) Continue(2);

                    var converted = new Token($"<=", TokenType.Symbol, Current().line, Current().column);
                    expression.Add(converted);
                }

                Continue();

                if (CurrentEquals("THAN", "TO")) Continue();
            }
            else
            {
                if (CurrentEquals("FUNCTION") || CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "("))
                {
                    var current = Current();
                    while (!CurrentEquals(")")) Continue();

                    Continue();
                    expression.Add(new Token("FUNCTION-CALL", TokenType.Identifier, current.line, current.column));
                }
                else
                {
                    expression.Add(Current());
                    Continue();
                }
            }
        }

        if (!Helpers.IsBalanced(expression))
        {
            ErrorHandler.Parser.Report(FileName, expression[0], ErrorType.General, """
            This expression is not balanced, one or more parenthesis to not have their matching opening or closing pair, it is an invalid expression
            """);
            ErrorHandler.Parser.PrettyError(FileName, expression[0]);
        }

        var shuntingYard = Helpers.ShuntingYard(expression, Helpers.BooleanPrecedence);

        if (!Helpers.EvaluatePostfix(shuntingYard, Helpers.BooleanPrecedence, out Token error))
        {
            ErrorHandler.Parser.Report(FileName, error, ErrorType.General, """
            This expression cannot be correctly evaluated. Please make sure that all operators have their matching operands.
            """);
            ErrorHandler.Parser.PrettyError(FileName, error);
        }
    }

    public static void StartRelationalOperator()
    {
        string[] operators =
        {
            "<",
            ">",
            "<=",
            ">=",
            "="
        };

        if (CurrentEquals("IS") && (LookaheadEquals(1, "GREATER", "LESS", "EQUAL", "NOT") || LookaheadEquals(1, TokenType.Symbol)))
        {
            Continue();
        }

        if (CurrentEquals("NOT") && LookaheadEquals(1, ">", "<"))
        {
            Continue(2);
        }
        else if (CurrentEquals("NOT") && LookaheadEquals(1, "GREATER", "LESS"))
        {
            Continue(2);

            if (CurrentEquals("THAN", "TO")) Continue();
        }
        else if (CurrentEquals("GREATER", "LESS", "EQUAL"))
        {
            if (CurrentEquals("GREATER") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
            {
                if (!LookaheadEquals(1, "THAN")) Continue(2);

                if (LookaheadEquals(1, "THAN")) Continue(3);
            }

            if (CurrentEquals("LESS") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
            {
                if (LookaheadEquals(1, "THAN")) Continue(3);

                if (!LookaheadEquals(1, "THAN")) Continue(2);

            }

            Continue();

            if (CurrentEquals("THAN", "TO")) Continue();
        }
        else if (CurrentEquals(operators))
        {
            Continue();
        }
        else
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, $"""
            Expected a relational operator. With the exceptions being the "IS NOT EQUAL TO" and "IS NOT =" operators 
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());

            Continue();
        }
    }

    public static void EncodingEndianness(bool encodingExists = false, bool endiannessExists = false)
    {
        if (CurrentEquals("BINARY-ENCODING", "DECIMAL-ENCODING"))
        {
            if (encodingExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The encoding phrase can only be specified once in this clause. 
                The same applies to the endianness phrase.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            encodingExists = true;
            Expected(Current().value);

            WriteBeforeAfter(encodingExists, endiannessExists);

        }

        if (CurrentEquals("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT"))
        {
            if (endiannessExists)
            {
                ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
                The endianness phrase can only be specified once in this clause. 
                The same applies to the encoding phrase.
                """);
                ErrorHandler.Parser.PrettyError(FileName, Current());
            }
            endiannessExists = true;
            Expected(Current().value);

            WriteBeforeAfter(encodingExists, endiannessExists);
        }
    }

    public static EvaluateOperand SelectionSubject()
    {
        if (CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String) && !LookaheadEquals(1, TokenType.Symbol))
        {
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
                return EvaluateOperand.Identifier;
            }

            ParseLiteral(true, true);
            return EvaluateOperand.Literal;
        }
        else if (CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String) && LookaheadEquals(1, TokenType.Symbol))
        {
            if (Helpers.ArithmeticPrecedence.ContainsKey(Lookahead(1).value))
            {
                Arithmetic("ALSO", "WHEN");
                return EvaluateOperand.Arithmetic;
            }
            else
            {
                Condition("ALSO", "WHEN");
                return EvaluateOperand.Condition;
            }
        }
        else if (CurrentEquals("TRUE", "FALSE"))
        {
            Choice("TRUE", "FALSE");
            return EvaluateOperand.TrueOrFalse;
        }

        return EvaluateOperand.Invalid;
    }

    public static void SelectionObject(EvaluateOperand operand)
    {
        bool identifier = operand is
            EvaluateOperand.Identifier or EvaluateOperand.Literal or
            EvaluateOperand.Arithmetic or EvaluateOperand.Boolean;

        bool literal = operand is
            EvaluateOperand.Identifier or EvaluateOperand.Arithmetic or 
            EvaluateOperand.Boolean;

        bool arithmetic = operand is
            EvaluateOperand.Identifier or EvaluateOperand.Literal or 
            EvaluateOperand.Arithmetic;

        bool boolean = operand is
            EvaluateOperand.Identifier or EvaluateOperand.Literal or 
            EvaluateOperand.Boolean;

        bool range = operand is
            EvaluateOperand.Identifier or EvaluateOperand.Literal or 
            EvaluateOperand.Arithmetic;

        bool condition = operand is 
            EvaluateOperand.Condition or EvaluateOperand.TrueOrFalse;
            
        bool truefalse = operand is
            EvaluateOperand.Condition or EvaluateOperand.TrueOrFalse;

        if (identifier || literal && CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String) && !LookaheadEquals(1, TokenType.Symbol))
        {
            if (identifier && CurrentEquals(TokenType.Identifier))
            {
                Identifier();
                RangeExpression(range, EvaluateOperand.Identifier);
            }
            else
            {
                ParseLiteral(true, true);
                RangeExpression(range, EvaluateOperand.Literal);
            }
        }
        else if (arithmetic || condition && CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String) && LookaheadEquals(1, TokenType.Symbol))
        {
            if (arithmetic && Helpers.ArithmeticPrecedence.ContainsKey(Lookahead(1).value))
            {
                Arithmetic("ALSO", "WHEN");
                RangeExpression(range, EvaluateOperand.Arithmetic);
            }
            else
            {
                Condition("ALSO", "WHEN");
            }
        }
        else if (truefalse && CurrentEquals("TRUE", "FALSE"))
        {
            Choice("TRUE", "FALSE");
        }
        else if (CurrentEquals("ANY"))
        {
            Expected("ANY");
        }
    }

    public static void RangeExpression(bool canHaveRange, EvaluateOperand rangeType)
    {
        if (canHaveRange && CurrentEquals("THROUGH", "THRU"))
        {
            Choice("THROUGH", "THRU");
            if (rangeType is EvaluateOperand.Identifier)
            {
                Identifier();
            }
            else if (rangeType is EvaluateOperand.Literal)
            {
                ParseLiteral(true, true);
            }
            else if (rangeType is EvaluateOperand.Arithmetic)
            {
                Arithmetic("ALSO", "WHEN");
            }

            if (CurrentEquals("IS", "UTF-8"))
            {
                Optional("IS");
                // Need to implement other alphabet support
                Expected("UTF-8");
            }
        }
    }

    public static void ParseLiteral(bool numeric, bool @string)
    {
        if (!CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String))
        {
            ErrorHandler.Parser.Report(FileName, Current(), ErrorType.General, """
            Expected an identifier or a literal
            """);
            ErrorHandler.Parser.PrettyError(FileName, Current());
        }

        if (numeric && CurrentEquals(TokenType.Numeric))
        {
            Number();
        }
        else if (@string && CurrentEquals(TokenType.String))
        {
            String();
        }
    }

    public static bool NotIdentifierOrLiteral()
    {
        return !CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String);
    }

    public static bool IdentifierOrLiteral()
    {
        return CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String);
    }

}