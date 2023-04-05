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

    private static void TimesPhrase()
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

    private static void UntilPhrase()
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

    private static void VaryingPhrase()
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

    private static void WithTest()
    {
        if (CurrentEquals("WITH", "TEST"))
        {
            Optional("WITH");
            Expected("TEST");
            Choice("BEFORE", "AFTER");
        }
    }

    private static void RetryPhrase()
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

    private static void TallyingPhrase()
    {
        if (!CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "FOR"))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                Tallying phrase, missing identifier.
                """)
            .WithSourceLine(Current(), """
                Tallying must start with an identifier, followed by the 'FOR' keyword.
                """)
            .CloseError();
        }

        while (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "FOR"))
        {
            Identifier();
            Expected("FOR");

            if (!CurrentEquals("CHARACTERS", "ALL", "LEADING"))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Tallying phrase, missing keyword.
                    """)
                .WithSourceLine(Current(), """
                    Missing phrase keyword.
                    """)
                .WithNote("""
                    Tallying must contain one of the following words: CHARACTERS, ALL or LEADING
                    """)
                .CloseError();
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

    private static void ReplacingPhrase()
    {
        if (!CurrentEquals("CHARACTERS", "ALL", "LEADING", "FIRST"))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                Replacing phrase, missing keyword.
                """)
            .WithSourceLine(Current(), """
                Missing phrase keyword.
                """)
            .WithNote("""
                Tallying must contain one of the following words: CHARACTERS, ALL, LEADING or FIRST.
                """)
            .CloseError();
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

    private static void AfterBeforePhrase(bool beforeExists = false, bool afterExists = false)
    {
        if (CurrentEquals("AFTER"))
        {
            if (afterExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    After phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    AFTER can only be specified once in this part of the statement.
                    """)
                .WithNote("""
                    The same applies to BEFORE.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Before phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    BEFORE can only be specified once in this part of the statement.
                    """)
                .WithNote("""
                    The same applies to AFTER.
                    """)
                .CloseError();
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

    private static void InvalidKey(ref bool isConditional, bool invalidKeyExists = false, bool notInvalidKeyExists = false)
    {
        if (CurrentEquals("INVALID"))
        {
            if (invalidKeyExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Invalid key phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    INVALID KEY can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to NOT INVALID KEY.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Not invalid key phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    NOT INVALID KEY can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to INVALID KEY.
                    """)
                .CloseError();
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

    private static void OnException(ref bool isConditional, bool onExceptionExists = false, bool notOnExceptionExists = false)
    {
        if (CurrentEquals("ON") || CurrentEquals("EXCEPTION"))
        {
            if (onExceptionExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    On exception phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    ON EXCEPTION can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to NOT ON EXCEPTION.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Not on exception phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    NOT ON EXCEPTION can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to ON EXCEPTION.
                    """)
                .CloseError();
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

    private static void RaisingStatus(bool raisingExists = false, bool statusExists = false)
    {
        if (CurrentEquals("RAISING"))
        {
            if (raisingExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Raising phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    Raising can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to WITH ... ERROR STATUS.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Error status phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    ERROR STATUS can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to RAISING.
                    """)
                .CloseError();
            }

            Optional("WITH");
            Choice("NORMAL", "ERROR");
            Optional("STATUS");
            switch (Current().Type)
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

    private static void AtEnd(ref bool isConditional, bool atEndExists = false, bool notAtEndExists = false)
    {
        if (CurrentEquals("AT") || CurrentEquals("END"))
        {
            if (atEndExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    At end phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    AT END can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to NOT AT END.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Not at end phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    NOT AT END can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to AT END.
                    """)
                .CloseError();
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

    private static void SizeError(ref bool isConditional, bool onErrorExists = false, bool notOnErrorExists = false)
    {
        if (CurrentEquals("ON") || CurrentEquals("SIZE"))
        {
            if (onErrorExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    On size error phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    ON SIZE ERROR can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to NOT ON SIZE ERROR.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Not on size error phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    NOT ON SIZE ERROR can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to ON SIZE ERROR.
                    """)
                .CloseError();
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

    private static void OnOverflow(ref bool isConditional, bool onOverflowExists = false, bool notOnOverflowExists = false)
    {
        if (CurrentEquals("ON") || CurrentEquals("OVERFLOW"))
        {
            if (onOverflowExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    On overflow phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    ON OVERFLOW can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to NOT ON OVERFLOW.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Not on overflow phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    NOT ON OVERFLOW can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to ON OVERFLOW.
                    """)
                .CloseError();
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

    private static void WriteBeforeAfter(bool beforeExists = false, bool afterExists = false)
    {
        if (CurrentEquals("BEFORE"))
        {
            if (beforeExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Before phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    BEFORE can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to AFTER.
                    """)
                .CloseError();
            }
            beforeExists = true;

            Expected("BEFORE");

            WriteBeforeAfter(beforeExists, afterExists);
        }

        if (CurrentEquals("AFTER"))
        {
            if (afterExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    After phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    AFTER can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to BEFORE.
                    """)
                .CloseError();
            }
            afterExists = true;

            Expected("AFTER");

            WriteBeforeAfter(beforeExists, afterExists);
        }
    }

    private static void SetLocale(SetLcValues locales = new())
    {
        if (CurrentEquals("LC_ALL"))
        {
            if (locales.LC_ALL)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132,"""
                    Locale phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    LC_ALL can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to each of the other locale names.
                    """)
                .CloseError();
            }
            locales.LC_ALL = true;

            Expected("LC_ALL");

            SetLocale(locales);
        }

        if (CurrentEquals("LC_COLLATE"))
        {
            if (locales.LC_COLLATE)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132,"""
                    Locale phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    LC_COLLATE can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to each of the other locale names.
                    """)
                .CloseError();
            }
            locales.LC_COLLATE = true;

            Expected("LC_COLLATE");

            SetLocale(locales);
        }

        if (CurrentEquals("LC_CTYPE"))
        {
            if (locales.LC_CTYPE)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132,"""
                    Locale phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    LC_CTYPE can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to each of the other locale names.
                    """)
                .CloseError();
            }
            locales.LC_CTYPE = true;

            Expected("LC_CTYPE");

            SetLocale(locales);
        }

        if (CurrentEquals("LC_MESSAGES"))
        {
            if (locales.LC_MESSAGES)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132,"""
                    Locale phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    LC_MESSAGES can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to each of the other locale names.
                    """)
                .CloseError();
            }
            locales.LC_MESSAGES = true;

            Expected("LC_MESSAGES");

            SetLocale(locales);
        }

        if (CurrentEquals("LC_MONETARY"))
        {
            if (locales.LC_MONETARY)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132,"""
                    Locale phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    LC_MONETARY can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to each of the other locale names.
                    """)
                .CloseError();
            }
            locales.LC_MONETARY = true;

            Expected("LC_MONETARY");

            SetLocale(locales);
        }

        if (CurrentEquals("LC_NUMERIC"))
        {
            if (locales.LC_NUMERIC)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132,"""
                    Locale phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    LC_NUMERIC can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to each of the other locale names.
                    """)
                .CloseError();
            }
            locales.LC_NUMERIC = true;

            Expected("LC_NUMERIC");

            SetLocale(locales);
        }

        if (CurrentEquals("LC_TIME"))
        {
            if (locales.LC_TIME)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132,"""
                    Locale phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    LC_TIME can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to each of the other locale names.
                    """)
                .CloseError();
            }
            locales.LC_TIME = true;

            Expected("LC_TIME");

            SetLocale(locales);
        }
    }

    private static void AtEndOfPage(ref bool isConditional, bool atEndOfPageExists = false, bool notAtEndOfPageExists = false)
    {
        if (CurrentEquals("AT", "END-OF-PAGE", "EOP"))
        {
            if (atEndOfPageExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    At end-of-page phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    AT END-OF-PAGE can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to NOT AT END-OF-PAGE.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Not at end-of-page phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    NOT AT END-OF-PAGE can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to AT END-OF-PAGE.
                    """)
                .CloseError();
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

    private static void ForAlphanumericForNational(bool forAlphanumericExists = false, bool forNationalExists = false)
    {
        if (CurrentEquals("FOR") && LookaheadEquals(1, "ALPHANUMERIC") || CurrentEquals("ALPHANUMERIC"))
        {
            if (forAlphanumericExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    For alphanumeric phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    FOR ALPHANUMERIC can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to FOR NATIONAL.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    For national phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    FOR NATIONAL can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to FOR ALPHANUMERIC.
                    """)
                .CloseError();
            }
            forNationalExists = true;

            Optional("FOR");
            Expected("NATIONAL");
            Optional("IS");

            Identifier();

            ForAlphanumericForNational(forAlphanumericExists, forNationalExists);
        }
    }

    private static void LineColumn(bool lineExists = false, bool columnExists = false)
    {
        if (CurrentEquals("LINE"))
        {
            if (lineExists)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Line number phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    LINE NUMBER can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to COLUMN NUMBER.
                    """)
                .CloseError();
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
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 132, """
                    Column number phrase, duplicate definition.
                    """)
                .WithSourceLine(Current(), """
                    COLUMN NUMBER can only be specified once in this statement.
                    """)
                .WithNote("""
                    The same applies to LINE NUMBER.
                    """)
                .CloseError();
            }
            columnExists = true;

            Expected(Current().Value);
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

    private static void Arithmetic(params string[] delimiter)
    {
        static bool IsArithmeticSymbol(Token current)
        {
            return ArithmeticPrecedence.ContainsKey(current.Value);
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
                ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                Invalid symbol in this arithmetic expression. Valid operators are: +, -, *, /, **, ( and )
                """);
                ErrorHandler.Analyzer.PrettyError(FileName, Current());
            }
        }

        if (!IsBalanced(expression))
        {
            ErrorHandler.Analyzer.Report(FileName, expression[0], ErrorType.General, """
            This expression is not balanced, one or more parenthesis to not have their matching opening or closing pair, it is an invalid expression
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, expression[0]);
        }

        var shuntingYard = ShuntingYard(expression, ArithmeticPrecedence);

        if (!EvaluatePostfix(shuntingYard, ArithmeticPrecedence, out Token error))
        {
            ErrorHandler.Analyzer.Report(FileName, error, ErrorType.General, """
            This expression cannot be correctly evaluated. Please make sure that all operators have their matching operands.
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, error);
        }
    }

    private static void Condition(params string[] delimiter)
    {
        var expression = new List<Token>();

        while (!CurrentEquals(TokenContext.IsStatement) && !CurrentEquals(delimiter))
        {
            if (CurrentEquals("IS") && (Lookahead(1).Value is "GREATER" or "LESS" or "EQUAL" or "NOT" || Lookahead(1).Type is TokenType.Symbol))
            {
                Continue();
            }
            else if (CurrentEquals("NOT") && (LookaheadEquals(1, ">") || LookaheadEquals(1, "<")))
            {
                var combined = new Token($"NOT {Lookahead(1).Value}", TokenType.Symbol, Current().Line, Current().Column);
                expression.Add(combined);
                Continue(2);
            }
            else if (CurrentEquals("NOT") && (LookaheadEquals(1, "GREATER") || LookaheadEquals(1, "LESS") || LookaheadEquals(1, "EQUAL")))
            {
                if (LookaheadEquals(1, "GREATER"))
                {
                    var combined = new Token($"NOT >", TokenType.Symbol, Current().Line, Current().Column);
                    expression.Add(combined);
                }

                if (LookaheadEquals(1, "LESS"))
                {
                    var combined = new Token($"NOT <", TokenType.Symbol, Current().Line, Current().Column);
                    expression.Add(combined);
                }

                if (LookaheadEquals(1, "EQUAL"))
                {
                    var combined = new Token($"<>", TokenType.Symbol, Current().Line, Current().Column);
                    expression.Add(combined);
                }

                Continue(2);

                if (CurrentEquals("THAN", "TO")) Continue();
            }
            else if (CurrentEquals("GREATER") || CurrentEquals("LESS") || CurrentEquals("EQUAL"))
            {
                if (CurrentEquals("GREATER"))
                {
                    var converted = new Token($">", TokenType.Symbol, Current().Line, Current().Column);
                    expression.Add(converted);
                }

                if (CurrentEquals("LESS"))
                {
                    var converted = new Token($"<", TokenType.Symbol, Current().Line, Current().Column);
                    expression.Add(converted);
                }

                if (CurrentEquals("EQUAL"))
                {
                    var converted = new Token($"=", TokenType.Symbol, Current().Line, Current().Column);
                    expression.Add(converted);
                }

                if (CurrentEquals("GREATER") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
                {
                    if (!LookaheadEquals(1, "THAN")) Continue(2);

                    if (LookaheadEquals(1, "THAN")) Continue(3);

                    var converted = new Token($">=", TokenType.Symbol, Current().Line, Current().Column);
                    expression.Add(converted);
                }

                if (CurrentEquals("LESS") && (LookaheadEquals(1, "OR") || LookaheadEquals(2, "OR")))
                {
                    if (LookaheadEquals(1, "THAN")) Continue(3);

                    if (!LookaheadEquals(1, "THAN")) Continue(2);

                    var converted = new Token($"<=", TokenType.Symbol, Current().Line, Current().Column);
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
                    expression.Add(new Token("FUNCTION-CALL", TokenType.Identifier, current.Line, current.Column));
                }
                else
                {
                    expression.Add(Current());
                    Continue();
                }
            }
        }

        if (!IsBalanced(expression))
        {
            ErrorHandler.Analyzer.Report(FileName, expression[0], ErrorType.General, """
            This expression is not balanced, one or more parenthesis to not have their matching opening or closing pair, it is an invalid expression
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, expression[0]);
        }

        var shuntingYard = ShuntingYard(expression, ConditionalPrecedence);

        if (!EvaluatePostfix(shuntingYard, ConditionalPrecedence, out Token error))
        {
            ErrorHandler.Analyzer.Report(FileName, error, ErrorType.General, """
            This expression cannot be correctly evaluated. Please make sure that all operators have their matching operands.
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, error);
        }
    }

    private static void StartRelationalOperator()
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
            ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, $"""
            Expected a relational operator. With the exceptions being the "IS NOT EQUAL TO" and "IS NOT =" operators 
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, Current());

            Continue();
        }
    }

    private static void EncodingEndianness(bool encodingExists = false, bool endiannessExists = false)
    {
        if (CurrentEquals("BINARY-ENCODING", "DECIMAL-ENCODING"))
        {
            if (encodingExists)
            {
                ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                The encoding phrase can only be specified once in this clause. 
                The same applies to the endianness phrase.
                """);
                ErrorHandler.Analyzer.PrettyError(FileName, Current());
            }
            encodingExists = true;
            Expected(Current().Value);

            WriteBeforeAfter(encodingExists, endiannessExists);

        }

        if (CurrentEquals("HIGH-ORDER-LEFT", "HIGH-ORDER-RIGHT"))
        {
            if (endiannessExists)
            {
                ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                The endianness phrase can only be specified once in this clause. 
                The same applies to the encoding phrase.
                """);
                ErrorHandler.Analyzer.PrettyError(FileName, Current());
            }
            endiannessExists = true;
            Expected(Current().Value);

            WriteBeforeAfter(encodingExists, endiannessExists);
        }
    }

    private static EvaluateOperand SelectionSubject()
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
            if (ArithmeticPrecedence.ContainsKey(Lookahead(1).Value))
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

    private static void SelectionObject(EvaluateOperand operand)
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
            else if (CurrentEquals("ANY"))
            {
                Expected("ANY");
            }
            else
            {
                ParseLiteral(true, true);
                RangeExpression(range, EvaluateOperand.Literal);
            }
        }
        else if (arithmetic || condition && CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String) && LookaheadEquals(1, TokenType.Symbol))
        {
            if (arithmetic && ArithmeticPrecedence.ContainsKey(Lookahead(1).Value))
            {
                Arithmetic("ALSO", "WHEN");
                RangeExpression(range, EvaluateOperand.Arithmetic);
            }
            else if (CurrentEquals("ANY"))
            {
                Expected("ANY");
            }
            else
            {
                Condition("ALSO", "WHEN");
            }
        }
        else if (truefalse && CurrentEquals("TRUE", "FALSE"))
        {
            Choice("TRUE", "FALSE", "ANY");
        }
        else if (CurrentEquals("ANY"))
        {
            Expected("ANY");
        }
    }

    private static void RangeExpression(bool canHaveRange, EvaluateOperand rangeType)
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

    private static void ParseLiteral(bool numeric, bool @string)
    {
        if (!CurrentEquals(TokenType.Identifier, TokenType.Numeric, TokenType.String))
        {
            ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
            Expected an identifier or a literal
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, Current());
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

    private static bool NotIdentifierOrLiteral()
    {
        return !IdentifierOrLiteral();
    }

    private static bool IdentifierOrLiteral()
    {
        return CurrentEquals(
            TokenType.Identifier,
            TokenType.Numeric,
            TokenType.String,
            TokenType.HexString,
            TokenType.Boolean,
            TokenType.HexBoolean,
            TokenType.National,
            TokenType.HexNational
        );
    }

}