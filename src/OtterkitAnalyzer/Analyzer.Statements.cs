namespace Otterkit;

public static partial class Analyzer
{
    // Analyzer Statement methods.
    // These are the methods used to parse and analyze all Standard COBOL statements.
    // All of these methods are responsible *ONLY* for statements, paragraphs and sections inside the procedure division.
    // There shouldn't be any identification, environment or data division methods here.

    private static void ParseStatements(bool isNested = false)
    {
        bool errorCheck = Current().Context != TokenContext.IsStatement
            && !(CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, ".") && !isNested)
            && !(CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION") && !isNested);

        if (errorCheck)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected start of a statement. Instead found {Current().Value}.
                """)
            .CloseError();

            AnchorPoint(TokenContext.IsStatement);
        }

        while (!isNested ? !CurrentEquals("EOF", "END") && !(CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION")) : CurrentEquals(TokenContext.IsStatement))
        {
            Statement(isNested);

            ScopeTerminator(isNested);

            if (isNested && !CurrentEquals(TokenContext.IsStatement)) return;

            if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION")) return;
        }
    }

    private static void Statement(bool isNested = false)
    {
        if (CurrentEquals("ACCEPT"))
            ACCEPT();

        else if (CurrentEquals("ADD"))
            ADD();

        else if (CurrentEquals("ALLOCATE"))
            ALLOCATE();

        else if (CurrentEquals("CALL"))
            CALL();

        else if (CurrentEquals("CANCEL"))
            CANCEL();

        else if (CurrentEquals("CLOSE"))
            CLOSE();

        else if (CurrentEquals("COMMIT"))
            COMMIT();

        else if (CurrentEquals("CONTINUE"))
            CONTINUE();

        else if (CurrentEquals("COMPUTE"))
            COMPUTE();

        else if (CurrentEquals("DISPLAY"))
            DISPLAY();

        else if (CurrentEquals("DIVIDE"))
            DIVIDE();

        else if (CurrentEquals("DELETE"))
            DELETE();

        else if (CurrentEquals("IF"))
            IF();

        else if (CurrentEquals("INITIALIZE"))
            INITIALIZE();

        else if (CurrentEquals("INITIATE"))
            INITIATE();

        else if (CurrentEquals("INSPECT"))
            INSPECT();

        else if (CurrentEquals("INVOKE"))
            INVOKE();

        else if (CurrentEquals("MERGE"))
            MERGE();

        else if (CurrentEquals("MULTIPLY"))
            MULTIPLY();

        else if (CurrentEquals("MOVE"))
            MOVE();

        else if (CurrentEquals("OPEN"))
            OPEN();

        else if (CurrentEquals("EXIT"))
            EXIT();

        else if (CurrentEquals("EVALUATE"))
            EVALUATE();

        else if (CurrentEquals("FREE"))
            FREE();

        else if (CurrentEquals("GENERATE"))
            GENERATE();

        else if (CurrentEquals("GO"))
            GO();

        else if (CurrentEquals("GOBACK"))
            GOBACK();

        else if (CurrentEquals("SUBTRACT"))
            SUBTRACT();

        else if (CurrentEquals("PERFORM"))
            PERFORM();

        else if (CurrentEquals("RELEASE"))
            RELEASE();

        else if (CurrentEquals("RAISE"))
            RAISE();

        else if (CurrentEquals("READ"))
            READ();

        else if (CurrentEquals("RECEIVE"))
            RECEIVE();

        else if (CurrentEquals("RESUME"))
            RESUME();

        else if (CurrentEquals("RETURN"))
            RETURN();

        else if (CurrentEquals("REWRITE"))
            REWRITE();

        else if (CurrentEquals("ROLLBACK"))
            ROLLBACK();

        else if (CurrentEquals("SEARCH"))
            SEARCH();

        else if (CurrentEquals("SEND"))
            SEND();

        else if (CurrentEquals("SET"))
            SET();

        else if (CurrentEquals("SORT"))
            SORT();

        else if (CurrentEquals("START"))
            START();

        else if (CurrentEquals("STOP"))
            STOP();

        else if (CurrentEquals("STRING"))
            STRING();

        else if (CurrentEquals("SUPPRESS"))
            SUPPRESS();

        else if (CurrentEquals("TERMINATE"))
            TERMINATE();

        else if (CurrentEquals("UNLOCK"))
            UNLOCK();

        else if (CurrentEquals("UNSTRING"))
            UNSTRING();

        else if (CurrentEquals("VALIDATE"))
            VALIDATE();

        else if (CurrentEquals("WRITE"))
            WRITE();

        else if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, ".") && !isNested)
            PARAGRAPH();
    }

    private static void DeclarativeProcedure()
    {
        if (CurrentEquals("DECLARATIVES"))
        {
            Expected("DECLARATIVES");
            Expected(".");

            Identifier();
            Expected("SECTION");
            Expected(".");
            UseStatement();
            ParseStatements();

            while (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION"))
            {
                Identifier();
                Expected("SECTION");
                Expected(".");
                UseStatement();
                ParseStatements();
            }

            Expected("END");
            Expected("DECLARATIVES");
            Expected(".");
        }

        while (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "SECTION"))
        {
            Identifier();
            Expected("SECTION");
            Expected(".");
            ParseStatements();
        }
    }

    private static void UseStatement()
    {
        Expected("USE");

        bool exceptionObject = CurrentEquals("AFTER") && LookaheadEquals(1, "EXCEPTION") && LookaheadEquals(2, "OBJECT")
            || CurrentEquals("AFTER") && LookaheadEquals(1, "EO")
            || CurrentEquals("EXCEPTION") && LookaheadEquals(1, "OBJECT")
            || CurrentEquals("EO");

        bool exceptionCondition = CurrentEquals("AFTER") && LookaheadEquals(1, "EXCEPTION") && LookaheadEquals(2, "CONDITION")
            || CurrentEquals("AFTER") && LookaheadEquals(1, "EC")
            || CurrentEquals("EXCEPTION") && LookaheadEquals(1, "CONDITION")
            || CurrentEquals("EC");

        bool reporting = CurrentEquals("GLOBAL") && LookaheadEquals(1, "BEFORE") || CurrentEquals("BEFORE");

        bool fileException = CurrentEquals("GLOBAL") && LookaheadEquals(1, "AFTER", "STANDARD", "EXCEPTION", "ERROR")
            || CurrentEquals("AFTER", "STANDARD", "EXCEPTION", "ERROR");

        if (exceptionObject)
        {
            Optional("AFTER");
            if (CurrentEquals("EO"))
            {
                Expected("EO");
            }
            else
            {
                Expected("EXCEPTION");
                Expected("OBJECT");
            }

            Identifier();
        }
        else if (exceptionCondition)
        {
            Optional("AFTER");
            if (CurrentEquals("EO"))
            {
                Expected("EO");
            }
            else
            {
                Expected("EXCEPTION");
                Expected("OBJECT");
            }

            Identifier();
        }
        else if (reporting)
        {
            if (CurrentEquals("GLOBAL")) Expected("GLOBAL");

            Expected("BEFORE");
            Expected("REPORTING");
            Identifier();
        }
        else if (fileException)
        {
            if (CurrentEquals("GLOBAL")) Expected("GLOBAL");

            Optional("AFTER");
            Optional("STANDARD");
            Choice("EXCEPTION", "ERROR");
            Optional("PROCEDURE");
            Optional("ON");

            if (CurrentEquals("INPUT", "OUTPUT", "I-O", "EXTEND"))
            {
                Choice("INPUT", "OUTPUT", "I-O", "EXTEND");
            }
            else
            {
                Identifier();
                while (CurrentEquals(TokenType.Identifier))
                    Identifier();
            }
        }
        else
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected AFTER EXCEPTION OBJECT, AFTER EXCEPTION CONDITION, BEFORE REPORTING or AFTER EXCEPTION/ERROR.
                """)
            .CloseError();

            AnchorPoint(TokenContext.IsStatement);
        }

        ScopeTerminator(false);
    }

    // This method handles COBOL's slightly inconsistent separator period rules.
    // Statements that are nested inside another statement cannot end with a separator period,
    // since that separator period would mean the end of the containing statement and not the contained statement.
    private static void ScopeTerminator(bool isNested)
    {
        if (isNested) return;

        if (CurrentEquals(".", ". "))
        {
            Expected(".");
            return;
        }

        if (!CurrentEquals(TokenContext.IsStatement))
        {
            Expected(".");
        }
    }


    // Statement parsing methods
    // All the following uppercased methods are responsible for parsing a single COBOL statement
    // When a new method is added here to parse a new statement, we need to add it to the Statement() method as well.
    // Adding extra statements to the parser only requires a new method here, and an if statement added to the Statement() method
    private static void DISPLAY()
    {
        Expected("DISPLAY");

        if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "AT", "LINE", "COLUMN", "COL"))
        {
            bool isConditional = false;

            Identifier();
            Optional("AT");
            LineColumn();
            OnException(ref isConditional);

            if (isConditional) Expected("END-COMPUTE");
            return;
        }

        if (NotIdentifierOrLiteral())
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected an identifier or a literal.
                """)
            .CloseError();

            AnchorPoint("UPON", "WITH", "NO");
        }

        switch (Current().Type)
        {
            case TokenType.Identifier: Identifier(); break;
            case TokenType.Numeric: Number(); break;
            
            case TokenType.String:
            case TokenType.HexString:
            case TokenType.Boolean:
            case TokenType.HexBoolean:
            case TokenType.National:
            case TokenType.HexNational:
                String(); break;
        }

        while (IdentifierOrLiteral())
        {
            switch (Current().Type)
            {
                case TokenType.Identifier: Identifier(); break;
                case TokenType.Numeric: Number(); break;
                
                case TokenType.String:
                case TokenType.HexString:
                case TokenType.Boolean:
                case TokenType.HexBoolean:
                case TokenType.National:
                case TokenType.HexNational:
                    String(); break;
            }
        }

        if (CurrentEquals("UPON"))
        {
            Expected("UPON");
            Choice("STANDARD-OUTPUT", "STANDARD-ERROR");
        }

        if (CurrentEquals("WITH", "NO"))
        {
            Optional("WITH");
            Expected("NO");
            Expected("ADVANCING");
        }

        Optional("END-DISPLAY");
    }

    private static void ACCEPT()
    {
        bool isConditional = false;

        Expected("ACCEPT");
        Identifier();
        if (CurrentEquals("FROM"))
        {
            Expected("FROM");
            
            if (CurrentEquals("STANDARD-INPUT", "COMMAND-LINE"))
            {
                    Choice("STANDARD-INPUT", "COMMAND-LINE");
            }
            else if (CurrentEquals("DATE"))
            {
                Expected("DATE");
                Optional("YYYYMMDD"); 
            }
            else if (CurrentEquals("DAY"))
            {
                Expected("DAY");
                Optional("YYYYDDD");   
            }
            else if (CurrentEquals("DAY-OF-WEEK"))
            {
                Expected("DAY-OF-WEEK");
            }
            else if (CurrentEquals("TIME"))
            {
                Expected("TIME");

            }
        }
        else if (CurrentEquals("AT", "LINE", "COLUMN", "COL"))
        {
            Optional("AT");
            if (!CurrentEquals("LINE", "COLUMN", "COL"))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Lookahead(-1).Type.Display(false)}.
                    """)
                .WithSourceLine(Lookahead(-1), $"""
                    When specifying the AT keyword, it must be followed by a LINE NUMBER, COLUMN/COL NUMBER or both.
                    """)
                .CloseError();
            }

            LineColumn();
            
            OnException(ref isConditional);
        }

        if (isConditional) Expected("END-ACCEPT");
    }

    private static void ALLOCATE()
    {
        Expected("ALLOCATE");
        if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "CHARACTERS") && !LookaheadEquals(1, TokenType.Symbol))
            Identifier();

        if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
        {
            Arithmetic("CHARACTERS");
            Expected("CHARACTERS");
        }

        if (CurrentEquals("INITIALIZED"))
            Expected("INITIALIZED");

        if (CurrentEquals("RETURNING"))
        {
            Expected("RETURNING");
            Identifier();
        }
    }

    private static void COMPUTE()
    {
        bool isConditional = false;

        Expected("COMPUTE");
        if (!CurrentEquals(TokenType.Identifier))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected an identifier.
                """)
            .CloseError();
        }

        while (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }

        Expected("=");
        if (NotIdentifierOrLiteral())
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected a valid arithmetic expression.
                """)
            .CloseError();
        }

        Arithmetic(".");

        if (CurrentEquals(".")) return;

        SizeError(ref isConditional);

        if (isConditional) Expected("END-COMPUTE");
    }

    private static void CALL()
    {
        bool isConditional = false;
        bool isPrototype = false;

        Expected("CALL");
        if (CurrentEquals(TokenType.Identifier, TokenType.String))
        {
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                String();
            }

            if (CurrentEquals("AS"))
            {
                isPrototype = true;
                Expected("AS");
            }
        }
        else
        {
            isPrototype = true;
        }

        if (isPrototype && CurrentEquals("NESTED"))
        {
            Expected("NESTED");
        }
        else if (isPrototype && !CurrentEquals("NESTED"))
        {
            Identifier();
        }

        if (!isPrototype && CurrentEquals("USING"))
        {
            Expected("USING");

            while (CurrentEquals("BY", "REFERENCE", "CONTENT"))
            {
                if (CurrentEquals("VALUE") || CurrentEquals("BY") && LookaheadEquals(1, "VALUE"))
                {
                    ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                    The USING BY VALUE clause must only be specified in a program prototype call.
                    """);
                    ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    Continue();

                    AnchorPoint("BY", "REFERENCE", "CONTENT", "RETURNING");
                    if (CurrentEquals("RETURNING", ".")) break;

                }

                if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && LookaheadEquals(1, "REFERENCE"))
                {
                    Optional("BY");
                    Expected("REFERENCE");

                    if (CurrentEquals("OPTIONAL")) Expected("OPTIONAL");

                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY REFERENCE clause must contain at least one data item name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    Identifier();
                    while (CurrentEquals(TokenType.Identifier) || CurrentEquals("OPTIONAL"))
                    {
                        if (CurrentEquals("OPTIONAL")) Expected("OPTIONAL");
                        Identifier();
                    }
                }

                if (CurrentEquals("CONTENT") || CurrentEquals("BY") && LookaheadEquals(1, "CONTENT"))
                {
                    Optional("BY");
                    Expected("CONTENT");
                    if (!CurrentEquals(TokenType.Identifier))
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY CONTENT clause must contain at least one data item name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());

                        AnchorPoint("BY", "REFERENCE", "CONTENT", "RETURNING");
                        if (CurrentEquals("RETURNING", ".")) break;
                    }

                    Identifier();
                    while (CurrentEquals(TokenType.Identifier)) Identifier();
                }
            }
        }
        else if (isPrototype && CurrentEquals("USING"))
        {
            Expected("USING");

            while (CurrentEquals("BY", "REFERENCE", "CONTENT", "VALUE"))
            {
                if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && LookaheadEquals(1, "REFERENCE"))
                {
                    Optional("BY");
                    Expected("REFERENCE");

                    if (!CurrentEquals(TokenType.Identifier) && !CurrentEquals("OMMITED"))
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY REFERENCE clause must contain at least one data item name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (CurrentEquals("OMMITED"))
                    {
                        Expected("OMMITED");
                    }
                    else
                    {
                        Identifier();
                    }
                }

                if (CurrentEquals("CONTENT") || CurrentEquals("BY") && LookaheadEquals(1, "CONTENT"))
                {
                    Optional("BY");
                    Expected("CONTENT");
                    if (NotIdentifierOrLiteral())
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY CONTENT clause must contain at least one data item name or literal.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else if (CurrentEquals(TokenType.Numeric))
                    {
                        Number();
                    }
                    else
                    {
                        String();
                    }
                }

                if (CurrentEquals("VALUE") || CurrentEquals("BY") && LookaheadEquals(1, "VALUE"))
                {
                    Optional("BY");
                    Expected("VALUE");
                    if (NotIdentifierOrLiteral())
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY VALUE clause must contain at least one data item name or literal.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else if (CurrentEquals(TokenType.Numeric))
                    {
                        Number();
                    }
                    else
                    {
                        String();
                    }
                }
            }
        }

        if (CurrentEquals("RETURNING"))
        {
            Expected("RETURNING");
            Identifier();
        }

        OnException(ref isConditional);

        if (isConditional) Expected("END-CALL");
    }

    private static void CONTINUE()
    {
        Expected("CONTINUE");
        if (CurrentEquals("AFTER"))
        {
            Expected("AFTER");
            Arithmetic("SECONDS");
            Expected("SECONDS");
        }
    }

    private static void ADD()
    {
        bool isConditional = false;

        Expected("ADD");

        if (CurrentEquals("CORRESPONDING", "CORR"))
        {
            Continue();
            Identifier();
            Expected("TO");
            Identifier();
            SizeError(ref isConditional);

            if (isConditional) Expected("END-ADD");
            return;
        }

        if (!CurrentEquals(TokenType.Identifier, TokenType.Numeric))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected an identifier or numeric literal.
                """)
            .CloseError();
        }

        while (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
        {
            if (CurrentEquals(TokenType.Identifier))
                Identifier();

            if (CurrentEquals(TokenType.Numeric))
                Number();
        }

        if (CurrentEquals("TO") && LookaheadEquals(2, "GIVING"))
        {
            Optional("TO");
            switch (Current().Type)
            {
                case TokenType.Identifier:
                    Identifier();
                    break;

                case TokenType.Numeric:
                    Number();
                    break;

                default:
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                        Unexpected {Current().Type.Display(false)}.
                        """)
                    .WithSourceLine(Current(), $"""
                        Expected an identifier or numeric literal.
                        """)
                    .CloseError();
                    break;
            }

            Expected("GIVING");
            if (Current().Type != TokenType.Identifier)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected an identifier.
                    """)
                .CloseError();
            }

            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else if (CurrentEquals("GIVING"))
        {
            Expected("GIVING");
            if (Current().Type != TokenType.Identifier)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected an identifier.
                    """)
                .CloseError();
            }

            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else if (CurrentEquals("TO"))
        {
            Expected("TO");
            if (Current().Type != TokenType.Identifier)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected an identifier.
                    """)
                .CloseError();
            }

            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected TO or GIVING reserved words.
                """)
            .CloseError();
        }

        SizeError(ref isConditional);

        if (isConditional) Expected("END-ADD");
    }

    private static void SUBTRACT()
    {
        bool isConditional = false;

        Expected("SUBTRACT");
        if (Current().Type != TokenType.Identifier && Current().Type != TokenType.Numeric)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected an identifier or numeric literal.
                """)
            .CloseError();
        }

        while (Current().Type == TokenType.Identifier
            || Current().Type == TokenType.Numeric
        )
        {
            if (Current().Type == TokenType.Identifier)
                Identifier();

            if (Current().Type == TokenType.Numeric)
                Number();
        }

        if (CurrentEquals("FROM") && LookaheadEquals(2, "GIVING"))
        {
            Optional("FROM");
            switch (Current().Type)
            {
                case TokenType.Identifier:
                    Identifier();
                    break;

                case TokenType.Numeric:
                    Number();
                    break;

                default:
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                        Unexpected {Current().Type.Display(false)}.
                        """)
                    .WithSourceLine(Current(), $"""
                        Expected an identifier or numeric literal.
                        """)
                    .CloseError();
                    break;
            }

            Expected("GIVING");
            if (Current().Type != TokenType.Identifier)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected an identifier.
                    """)
                .CloseError();
            }

            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else if (CurrentEquals("FROM"))
        {
            Expected("FROM");
            if (Current().Type != TokenType.Identifier)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected an identifier.
                    """)
                .CloseError();
            }

            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected FROM reserved word.
                """)
            .CloseError();
        }

        SizeError(ref isConditional);

        if (isConditional)
            Expected("END-SUBTRACT");
    }

    private static void IF()
    {
        Expected("IF");
        Condition("THEN");
        Optional("THEN");
        if (CurrentEquals("NEXT") && LookaheadEquals(1, "SENTENCE"))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unsupported phrase: NEXT SENTENCE is an archaic feature.
                """)
            .WithSourceLine(Current(), $"""
                The CONTINUE statement can be used to accomplish the same functionality.
                """)
            .CloseError();
        }
        
        ParseStatements(true);

        if (CurrentEquals("ELSE"))
        {
            Expected("ELSE");
            ParseStatements(true);
        }

        Expected("END-IF");
    }

    private static void INITIALIZE()
    {
        Expected("INITIALIZE");
        Identifier();
        while (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }

        if (CurrentEquals("WITH", "FILLER"))
        {
            Optional("WITH");
            Expected("FILLER");
        }

        static bool IsCategoryName()
        {
            if (CurrentEquals(
            "ALPHABETIC",
            "ALPHANUMERIC",
            "ALPHANUMERIC-EDITED",
            "BOOLEAN",
            "DATA-POINTER",
            "FUNCTION-POINTER",
            "MESSAGE-TAG",
            "NATIONAL",
            "NATIONAL-EDITED",
            "NUMERIC",
            "NUMERIC-EDITED",
            "OBJECT-REFERENCE",
            "PROGRAM-POINTER"
            )) return true;

            return false;
        };

        if (IsCategoryName())
        {
            Expected(Current().Value);
            Optional("TO");
            Expected("VALUE");
        }
        else if (CurrentEquals("ALL"))
        {
            Expected("ALL");
            Optional("TO");
            Expected("VALUE");
        }

        if (CurrentEquals("THEN", "REPLACING"))
        {
            Optional("THEN");
            Expected("REPLACING");
            if (IsCategoryName())
            {
                Expected(Current().Value);
                Optional("DATA");
                Expected("BY");

                if (NotIdentifierOrLiteral())
                {
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                        Unexpected {Current().Type.Display(false)}.
                        """)
                    .WithSourceLine(Current(), $"""
                        Expected an identifier or literal.
                        """)
                    .CloseError();

                    AnchorPoint(TokenContext.IsStatement);
                }

                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else if (CurrentEquals(TokenType.String))
                {
                    String();
                }
                else if (CurrentEquals(TokenType.Numeric))
                {
                    Number();
                }

                while (IsCategoryName())
                {
                    Expected(Current().Value);
                    Optional("DATA");
                    Expected("BY");

                    if (NotIdentifierOrLiteral())
                    {
                        Error
                        .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                            Unexpected {Current().Type.Display(false)}.
                            """)
                        .WithSourceLine(Current(), $"""
                            Expected an identifier or literal.
                            """)
                        .CloseError();

                        AnchorPoint(TokenContext.IsStatement);
                    }

                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else if (CurrentEquals(TokenType.String))
                    {
                        String();
                    }
                    else if (CurrentEquals(TokenType.Numeric))
                    {
                        Number();
                    }

                }

            }
        }

        if (CurrentEquals("THEN", "TO", "DEFAULT"))
        {
            Optional("THEN");
            Optional("TO");
            Expected("DEFAULT");
        }
    }

    private static void INITIATE()
    {
        Expected("INITIATE");
        if (Current().Type != TokenType.Identifier)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected a report entry identifier defined in the report section.
                """)
            .CloseError();
        }
        Identifier();
        while (Current().Type == TokenType.Identifier)
            Identifier();

        if (!CurrentEquals("."))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected a report entry identifier defined in the report section.
                """)
            .CloseError();
        }
    }

    private static void INSPECT()
    {
        Expected("INSPECT");
        if (CurrentEquals("BACKWARD")) Expected("BACKWARD");

        Identifier();

        if (CurrentEquals("CONVERTING"))
        {
            Expected("CONVERTING");
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                String();
            }

            Expected("TO");
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                String();
            }

            AfterBeforePhrase();
        }
        else if (CurrentEquals("REPLACING"))
        {
            Expected("REPLACING");
            ReplacingPhrase();
        }
        else if (CurrentEquals("TALLYING"))
        {
            Expected("TALLYING");
            TallyingPhrase();
            if (CurrentEquals("REPLACING"))
            {
                Expected("REPLACING");
                ReplacingPhrase();
            }
        }
    }

    private static void INVOKE()
    {
        Expected("INVOKE");
        Identifier(UsageType.ObjectReference);
        if (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
        }
        else
        {
            String();
        }

        if (CurrentEquals("USING"))
        {
            Expected("USING");

            while (CurrentEquals("BY", "REFERENCE", "CONTENT", "VALUE"))
            {
                if (CurrentEquals("REFERENCE") || CurrentEquals("BY") && LookaheadEquals(1, "REFERENCE"))
                {
                    Optional("BY");
                    Expected("REFERENCE");

                    if (!CurrentEquals(TokenType.Identifier) && !CurrentEquals("OMMITED"))
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY REFERENCE clause must contain at least one data item name.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (CurrentEquals("OMMITED"))
                    {
                        Expected("OMMITED");
                    }
                    else
                    {
                        Identifier();
                    }
                }

                if (CurrentEquals("CONTENT") || CurrentEquals("BY") && LookaheadEquals(1, "CONTENT"))
                {
                    Optional("BY");
                    Expected("CONTENT");
                    if (NotIdentifierOrLiteral())
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY CONTENT clause must contain at least one data item name or literal.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else if (CurrentEquals(TokenType.Numeric))
                    {
                        Number();
                    }
                    else
                    {
                        String();
                    }
                }

                if (CurrentEquals("VALUE") || CurrentEquals("BY") && LookaheadEquals(1, "VALUE"))
                {
                    Optional("BY");
                    Expected("VALUE");
                    if (NotIdentifierOrLiteral())
                    {
                        ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                        The USING BY VALUE clause must contain at least one data item name or literal.
                        """);
                        ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    }

                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier();
                    }
                    else if (CurrentEquals(TokenType.Numeric))
                    {
                        Number();
                    }
                    else
                    {
                        String();
                    }
                }
            }
        }

        if (CurrentEquals("RETURNING"))
        {
            Expected("RETURNING");
            Identifier();
        }
    }

    private static void MERGE()
    {
        Expected("MERGE");
        Identifier();

        Optional("ON");
        if (CurrentEquals("ASCENDING"))
        {
            Expected("ASCENDING");
        }
        else
        {
            Expected("DESCENDING");
        }

        Optional("KEY");

        if (!CurrentEquals(TokenType.Identifier))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                The ON ASCENDING / DESCENDING KEY phrase must only contain key names.
                """)
            .CloseError();
        }

        Identifier();
        while (Current().Type == TokenType.Identifier)
            Identifier();


        while (CurrentEquals("ON", "ASCENDING", "DESCENDING"))
        {
            Optional("ON");
            if (CurrentEquals("ASCENDING"))
            {
                Expected("ASCENDING");
            }
            else
            {
                Expected("DESCENDING");
            }

            Optional("KEY");

            if (!CurrentEquals(TokenType.Identifier))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    The ON ASCENDING / DESCENDING KEY phrase must only contain key names.
                    """)
                .CloseError();
            }

            Identifier();
            while (Current().Type == TokenType.Identifier)
                Identifier();
        }

        if (CurrentEquals("COLLATING", "SEQUENCE"))
        {
            Optional("COLLATING");
            Expected("SEQUENCE");
            if (CurrentEquals("IS") && LookaheadEquals(1, TokenType.Identifier) || CurrentEquals(TokenType.Identifier))
            {
                Optional("IS");
                Identifier();

                if (CurrentEquals(TokenType.Identifier)) Identifier();
            }
            else
            {
                if (!CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
                {
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                        Unexpected {Current().Type.Display(false)}.
                        """)
                    .WithSourceLine(Current(), $"""
                        Expected an alphabet name or at least one FOR ALPHANUMERIC and FOR NATIONAL phrases.
                        """)
                    .CloseError();

                    CombinedAnchorPoint(TokenContext.IsStatement, "USING");
                }

                ForAlphanumericForNational();
            }
        }

        Expected("USING");
        Identifier();
        Identifier();
        while (Current().Type == TokenType.Identifier)
            Identifier();

        if (CurrentEquals("OUTPUT"))
        {
            Expected("OUTPUT");
            Expected("PROCEDURE");
            Optional("IS");
            Identifier();

            if (CurrentEquals("THROUGH", "THRU"))
            {
                Choice("THROUGH", "THRU");
                Identifier();
            }
        }
        else
        {
            Expected("GIVING");
            Identifier();
            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
    }

    private static void MULTIPLY()
    {
        bool isConditional = false;

        Expected("MULTIPLY");
        switch (Current().Type)
        {
            case TokenType.Identifier:
                Identifier();
                break;

            case TokenType.Numeric:
                Number();
                break;

            default:
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected an identifier or numeric literal.
                    """)
                .CloseError();
                break;
        }

        if (CurrentEquals("BY") && LookaheadEquals(2, "GIVING"))
        {
            Optional("BY");
            switch (Current().Type)
            {
                case TokenType.Identifier:
                    Identifier();
                    break;

                case TokenType.Numeric:
                    Number();
                    break;

                default:
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                        Unexpected {Current().Type.Display(false)}.
                        """)
                    .WithSourceLine(Current(), $"""
                        Expected an identifier or numeric literal.
                        """)
                    .CloseError();
                    break;
            }

            Expected("GIVING");
            if (Current().Type != TokenType.Identifier)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected an identifier.
                    """)
                .CloseError();
            }

            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else if (CurrentEquals("BY"))
        {
            Expected("BY");
            if (Current().Type != TokenType.Identifier)
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected an identifier.
                    """)
                .CloseError();
            }

            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected BY reserved word.
                """)
            .CloseError();
        }

        SizeError(ref isConditional);

        if (isConditional)
            Expected("END-MULTIPLY");
    }

    private static void MOVE()
    {
        Expected("MOVE");
        if (CurrentEquals("CORRESPONDING") || CurrentEquals("CORR"))
        {
            Expected(Current().Value);
            Identifier();
            Expected("TO");
            Identifier();
            return;
        }

        if (NotIdentifierOrLiteral())
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected a single data item identifier, literal or a function.
                """)
            .CloseError();
        }

        if (Current().Type == TokenType.Identifier)
            Identifier();

        else if (Current().Type == TokenType.Numeric)
            Number();

        else if (Current().Type == TokenType.String)
            String();

        Expected("TO");
        if (Current().Type != TokenType.Identifier)
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected only data item identifiers.
                """)
            .CloseError();
        }

        while (Current().Type == TokenType.Identifier)
            Identifier();

        if (!CurrentEquals(".") && !CurrentEquals(TokenType.ReservedKeyword) && !CurrentEquals(TokenContext.IsStatement))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected only data item identifiers.
                """)
            .CloseError();
        }
    }

    private static void OPEN()
    {
        Expected("OPEN");
        Choice("INPUT", "OUTPUT", "I-O", "EXTEND");

        if (CurrentEquals("SHARING"))
        {
            Expected("SHARING");
            Optional("WITH");
            if (CurrentEquals("ALL"))
            {
                Expected("ALL");
                Optional("OTHER");
            }
            else if (CurrentEquals("NO"))
            {
                Expected("NO");
                Optional("OTHER");
            }
            else if (CurrentEquals("READ"))
            {
                Expected("READ");
                Expected("ONLY");
            }
            else
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected ALL OTHER, NO OTHER or READ ONLY.
                    """)
                .WithNote("""
                    One of them must be specified in the SHARING phrase.
                    """)
                .CloseError();
            }
        }

        if (CurrentEquals("RETRY"))
        {
            RetryPhrase();
        }

        Identifier();
        if (CurrentEquals("WITH", "NO"))
        {
            Optional("WITH");
            Expected("NO");
            Expected("REWIND");
        }

        while (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
            if (CurrentEquals("WITH", "NO"))
            {
                Optional("WITH");
                Expected("NO");
                Expected("REWIND");
            }
        }

        while (CurrentEquals("INPUT", "OUTPUT", "I-O", "EXTEND"))
        {
            Choice("INPUT", "OUTPUT", "I-O", "EXTEND");

            if (CurrentEquals("SHARING"))
            {
                Expected("SHARING");
                Optional("WITH");
                if (CurrentEquals("ALL"))
                {
                    Expected("ALL");
                    Optional("OTHER");
                }
                else if (CurrentEquals("NO"))
                {
                    Expected("NO");
                    Optional("OTHER");
                }
                else if (CurrentEquals("READ"))
                {
                    Expected("READ");
                    Expected("ONLY");
                }
                else
                {
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                        Unexpected {Current().Type.Display(false)}.
                        """)
                    .WithSourceLine(Current(), $"""
                        Expected ALL OTHER, NO OTHER or READ ONLY.
                        """)
                    .WithNote("""
                        One of them must be specified in the SHARING phrase.
                        """)
                    .CloseError();
                }
            }

            if (CurrentEquals("RETRY"))
            {
                RetryPhrase();
            }

            Identifier();
            if (CurrentEquals("WITH", "NO"))
            {
                Optional("WITH");
                Expected("NO");
                Expected("REWIND");
            }

            while (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
                if (CurrentEquals("WITH", "NO"))
                {
                    Optional("WITH");
                    Expected("NO");
                    Expected("REWIND");
                }
            }

        }
    }

    private static void DIVIDE()
    {
        bool isConditional = false;

        Expected("DIVIDE");
        switch (Current().Type)
        {
            case TokenType.Identifier:
                Identifier();
                break;

            case TokenType.Numeric:
                Number();
                break;

            default:
                ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                ErrorHandler.Analyzer.PrettyError(FileName, Current());
                break;
        }

        if ((CurrentEquals("BY") || CurrentEquals("INTO")) && LookaheadEquals(2, "GIVING") && !LookaheadEquals(4, "REMAINDER"))
        {
            Choice("BY", "INTO");
            switch (Current().Type)
            {
                case TokenType.Identifier:
                    Identifier();
                    break;

                case TokenType.Numeric:
                    Number();
                    break;

                default:
                    ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                    ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    break;
            }

            Expected("GIVING");
            if (Current().Type != TokenType.Identifier)
            {
                ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.Expected, "identifier");
                ErrorHandler.Analyzer.PrettyError(FileName, Current());
            }

            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else if ((CurrentEquals("BY") || CurrentEquals("INTO")) && LookaheadEquals(2, "GIVING") && LookaheadEquals(4, "REMAINDER"))
        {
            Choice("BY", "INTO");
            switch (Current().Type)
            {
                case TokenType.Identifier:
                    Identifier();
                    break;

                case TokenType.Numeric:
                    Number();
                    break;

                default:
                    ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.Expected, "identifier or numeric literal");
                    ErrorHandler.Analyzer.PrettyError(FileName, Current());
                    break;
            }

            Expected("GIVING");
            Identifier();
            Expected("REMAINDER");
            Identifier();
        }
        else if (CurrentEquals("INTO"))
        {
            Expected("INTO");
            if (Current().Type != TokenType.Identifier)
            {
                ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.Expected, "identifier");
                ErrorHandler.Analyzer.PrettyError(FileName, Current());
            }

            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else
        {
            ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.Expected, "BY or INTO");
            ErrorHandler.Analyzer.PrettyError(FileName, Current());
        }

        SizeError(ref isConditional);

        if (isConditional)
            Expected("END-MULTIPLY");
    }

    private static void DELETE()
    {
        bool isConditional = false;
        bool isFile = false;

        Expected("DELETE");
        if (CurrentEquals("FILE"))
        {
            isFile = true;
            Expected("FILE");
            Optional("OVERRIDE");
            Identifier();
            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
        else if (Current().Type == TokenType.Identifier)
        {
            Identifier();
            Expected("RECORD");
        }

        if (CurrentEquals("RETRY"))
            RetryPhrase();

        if (!isFile)
            InvalidKey(ref isConditional);

        if (isFile)
            OnException(ref isConditional);

        if (isConditional)
            Expected("END-DELETE");
    }

    private static void EVALUATE()
    {
        var conditions = new List<EvaluateOperand>();
        var conditionsIndex = 0;
        Expected("EVALUATE");

        conditions.Add(SelectionSubject());
        while (CurrentEquals("ALSO"))
        {
            Expected("ALSO");
            conditions.Add(SelectionSubject());
        }

        Expected("WHEN");
        SelectionObject(conditions[conditionsIndex]);
        conditionsIndex++;

        while (CurrentEquals("ALSO"))
        {
            Expected("ALSO");
            SelectionObject(conditions[conditionsIndex]);
            conditionsIndex++;
        }
        conditionsIndex = 0;

        ParseStatements(true);

        while (CurrentEquals("WHEN") && !LookaheadEquals(1, "OTHER"))
        {
            Expected("WHEN");
            SelectionObject(conditions[conditionsIndex]);
            conditionsIndex++;

            while (CurrentEquals("ALSO"))
            {
                Expected("ALSO");
                SelectionObject(conditions[conditionsIndex]);
                conditionsIndex++;
            }
            conditionsIndex = 0;

            ParseStatements(true);
        }

        if (CurrentEquals("WHEN") && LookaheadEquals(1, "OTHER"))
        {
            Expected("WHEN");
            Expected("OTHER");
            ParseStatements(true);
        }

        Expected("END-EVALUATE");
    }

    private static void EXIT()
    {
        Expected("EXIT");
        if (CurrentEquals("PERFORM"))
        {
            Expected("PERFORM");
            Optional("CYCLE");
        }
        else if (CurrentEquals("PARAGRAPH"))
            Expected("PARAGRAPH");

        else if (CurrentEquals("SECTION"))
            Expected("SECTION");

        else if (CurrentEquals("PROGRAM"))
        {
            Expected("PROGRAM");
            if (CurrentEquals("RAISING"))
            {
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
            }
        }
    }

    private static void FREE()
    {
        Expected("FREE");
        Identifier();
        while (CurrentEquals(TokenType.Identifier)) Identifier();

        if (!CurrentEquals("."))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected a data item defined with the BASED clause.
                """)
            .CloseError();
        }

    }

    private static void GENERATE()
    {
        Expected("GENERATE");
        Identifier();
    }

    private static void GO()
    {
        Expected("GO");
        Optional("TO");
        Identifier();
        if (CurrentEquals("DEPENDING") || Current().Type == TokenType.Identifier)
        {
            while (CurrentEquals(TokenType.Identifier)) Identifier();

            Expected("DEPENDING");
            Optional("ON");
            Identifier();
        }
    }

    private static void GOBACK()
    {
        Expected("GOBACK");
        RaisingStatus();
    }

    private static void COMMIT()
    {
        Expected("COMMIT");
    }

    private static void CLOSE()
    {
        Expected("CLOSE");
        if (Current().Type == TokenType.Identifier)
        {
            Identifier();
            if (CurrentEquals("REEL") || CurrentEquals("UNIT"))
            {
                Expected(Current().Value);

                if (CurrentEquals("FOR") || CurrentEquals("REMOVAL"))
                {
                    Optional("FOR");
                    Expected("REMOVAL");
                }
            }
            else if (CurrentEquals("WITH") || CurrentEquals("NO"))
            {
                Optional("WITH");
                Expected("NO");
                Expected("REWIND");
            }
        }
        else
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected a file connector name.
                """)
            .CloseError();
        }

        while (Current().Type == TokenType.Identifier)
        {
            Identifier();
            if (CurrentEquals("REEL", "UNIT"))
            {
                Expected(Current().Value);

                if (CurrentEquals("FOR", "REMOVAL"))
                {
                    Optional("FOR");
                    Expected("REMOVAL");
                }
            }
            else if (CurrentEquals("WITH", "NO"))
            {
                Optional("WITH");
                Expected("NO");
                Expected("REWIND");
            }
        }

        if (!CurrentEquals("."))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected a file connector name.
                """)
            .CloseError();
        }
    }

    private static void CANCEL()
    {
        Expected("CANCEL");
        if (Current().Type == TokenType.Identifier)
            Identifier();

        else if (Current().Type == TokenType.String)
            String();

        else
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected an alphanumeric or national literal, or a REPOSITORY paragraph program name.
                """)
            .CloseError();

            Continue();
        }

        while (Current().Type == TokenType.Identifier || Current().Type == TokenType.String)
        {
            if (Current().Type == TokenType.Identifier)
                Identifier();

            if (Current().Type == TokenType.String)
                String();
        }

        if (!CurrentEquals("."))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                Expected an alphanumeric or national literal, or a REPOSITORY paragraph program name.
                """)
            .CloseError();
        }
    }

    private static void PERFORM()
    {
        bool isExceptionChecking = false;
        bool isInline = false;

        Expected("PERFORM");
        if (CurrentEquals(TokenType.Identifier))
        {
            Identifier();
            if (CurrentEquals("THROUGH", "THRU"))
            {
                Choice("THROUGH", "THRU");
                Identifier();
            }

            if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
            {
                TimesPhrase();
            }
            else if (CurrentEquals("WITH", "TEST", "VARYING", "UNTIL"))
            {
                WithTest();
                if (CurrentEquals("VARYING"))
                {
                    VaryingPhrase();
                }
                else if (CurrentEquals("UNTIL"))
                {
                    UntilPhrase();
                }
            }
        }
        else
        {
            if (CurrentEquals("LOCATION") || CurrentEquals("WITH") && LookaheadEquals(1, "LOCATION"))
            {
                isExceptionChecking = true;
                Optional("WITH");
                Expected("LOCATION");
            }
            else if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
            {
                isInline = true;

                TimesPhrase();
            }
            else if (CurrentEquals("TEST", "VARYING", "UNTIL") || CurrentEquals("WITH") && LookaheadEquals(1, "TEST"))
            {
                isInline = true;

                WithTest();
                if (CurrentEquals("VARYING"))
                {
                    VaryingPhrase();
                }
                else if (CurrentEquals("UNTIL"))
                {
                    UntilPhrase();
                }
            }

            ParseStatements(true);

            if (isInline && CurrentEquals("WHEN"))
            {
                ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.Recovery, """
                An inline PERFORM with a TIMES, VARYING or UNTIL phrase cannot contain an exception checking WHEN phrase 
                """);
                ErrorHandler.Analyzer.PrettyError(FileName, Current(), ConsoleColor.Blue);

                AnchorPoint("END-PERFORM");
            }
            
            if (isExceptionChecking || !isInline && CurrentEquals("WHEN"))
            {
                isExceptionChecking = true;

                Expected("WHEN");
                if (CurrentEquals("EXCEPTION") && LookaheadEquals(1, TokenType.Identifier))
                {
                    Expected("EXCEPTION");

                    Identifier();
                    while (CurrentEquals(TokenType.Identifier))
                        Identifier();

                    ParseStatements(true);
                }
                else if (CurrentEquals("EXCEPTION"))
                {
                    Expected("EXCEPTION");
                    Choice("INPUT", "OUTPUT", "IO", "EXTEND");
                    ParseStatements(true);
                }
                else if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "FILE"))
                {
                    Identifier();
                    Expected("FILE");
                    Identifier();

                    while (CurrentEquals(TokenType.Identifier))
                        Identifier();
                }
                else if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "FILE"))
                {
                    Identifier();
                    while (CurrentEquals(TokenType.Identifier))
                        Identifier();
                }

                while (CurrentEquals("WHEN"))
                {
                    Expected("WHEN");
                    if (CurrentEquals("EXCEPTION") && LookaheadEquals(1, TokenType.Identifier))
                    {
                        Expected("EXCEPTION");

                        Identifier();
                        while (CurrentEquals(TokenType.Identifier))
                            Identifier();

                        ParseStatements(true);
                    }
                    else if (CurrentEquals("EXCEPTION"))
                    {
                        Expected("EXCEPTION");
                        Choice("INPUT", "OUTPUT", "IO", "EXTEND");
                        ParseStatements(true);
                    }
                    else if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "FILE"))
                    {
                        Identifier();
                        Expected("FILE");
                        Identifier();

                        while (CurrentEquals(TokenType.Identifier))
                            Identifier();
                    }
                    else if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, "FILE"))
                    {
                        Identifier();
                        while (CurrentEquals(TokenType.Identifier))
                            Identifier();
                    }
                }
            }

            if (isExceptionChecking && CurrentEquals("WHEN") && LookaheadEquals(1, "OTHER"))
            {
                Expected("WHEN");
                Expected("OTHER");
                Optional("EXCEPTION");

                ParseStatements(true);
            }

            if (isExceptionChecking && CurrentEquals("WHEN", "COMMON"))
            {
                Optional("WHEN");
                Expected("COMMON");
                Optional("EXCEPTION");

                ParseStatements(true);
            }

            if (isExceptionChecking && CurrentEquals("FINALLY"))
            {
                Expected("FINALLY");

                ParseStatements(true);
            }

            Expected("END-PERFORM");
        }
    }

    private static void RAISE()
    {
        Expected("RAISE");
        if (CurrentEquals("EXCEPTION"))
        {
            Expected("EXCEPTION");
            Identifier();
        }
        else
            Identifier();
    }

    private static void READ()
    {
        bool isSequential = false;
        bool isConditional = false;

        Expected("READ");
        Identifier();
        if (CurrentEquals("NEXT", "PREVIOUS"))
        {
            Expected(Current().Value);
            isSequential = true;
        }

        Expected("RECORD");
        if (CurrentEquals("INTO"))
        {
            Expected("INTO");
            Identifier();
        }

        if (CurrentEquals("ADVANCING"))
        {
            Expected("ADVANCING");
            Optional("ON");
            Expected("LOCK");
            isSequential = true;
        }
        else if (CurrentEquals("IGNORING"))
        {
            Expected("IGNORING");
            Expected("LOCK");
        }
        else if (CurrentEquals("RETRY"))
        {
            RetryPhrase();
        }

        if (CurrentEquals("WITH", "LOCK"))
        {
            Optional("WITH");
            Expected("LOCK");
        }
        else if (CurrentEquals("WITH", "NO"))
        {
            Optional("WITH");
            Expected("NO");
            Expected("LOCK");
        }

        if (!isSequential && CurrentEquals("KEY"))
        {
            Expected("KEY");
            Optional("IS");
            Identifier();
        }

        if (!isSequential && CurrentEquals("INVALID", "NOT"))
        {
            InvalidKey(ref isConditional);
        }
        else if (isSequential && CurrentEquals("AT", "END", "NOT"))
        {
            AtEnd(ref isConditional);
        }

        if (isConditional) Expected("END-READ");
    }

    private static void RECEIVE()
    {
        bool isConditional = false;

        Expected("RECEIVE");
        Optional("FROM");
        Identifier();
        Expected("GIVING");
        Identifier();
        Identifier();

        if (CurrentEquals("CONTINUE"))
        {
            Expected("CONTINUE");
            Optional("AFTER");
            if (CurrentEquals("MESSAGE"))
            {
                Expected("MESSAGE");
                Expected("RECEIVED");
            }
            else
            {
                Arithmetic("SECONDS");
                Optional("SECONDS");
            }
        }

        OnException(ref isConditional);

        if (isConditional) Expected("END-RECEIVE");

    }

    private static void RELEASE()
    {
        Expected("RELEASE");
        Identifier();

        if (CurrentEquals("FROM"))
        {
            Expected("FROM");
            if (Current().Type == TokenType.String)
                String();

            else if (Current().Type == TokenType.Numeric)
                Number();

            else
                Identifier();
        }
    }

    private static void RETURN()
    {
        bool isConditional = false;

        Expected("RETURN");
        Identifier();
        Expected("RECORD");
        if (CurrentEquals("INTO"))
        {
            Expected("INTO");
            Identifier();
        }

        AtEnd(ref isConditional);

        if (isConditional)
            Expected("END-RETURN");
    }

    private static void REWRITE()
    {
        bool isConditional = false;
        bool isFile = false;

        Expected("REWRITE");
        if (CurrentEquals("FILE"))
        {
            isFile = true;
            Expected("FILE");
            Identifier();
        }
        else
            Identifier();

        Expected("RECORD");
        if (CurrentEquals("FROM") || isFile)
        {
            Expected("FROM");

            if (Current().Type == TokenType.Identifier)
                Identifier();

            else if (Current().Type == TokenType.Numeric)
                Number();

            else
                String();
        }

        RetryPhrase();
        if (CurrentEquals("WITH") || CurrentEquals("LOCK") || CurrentEquals("NO"))
        {
            Optional("WITH");
            if (CurrentEquals("LOCK"))
            {
                Expected("LOCK");
            }
            else
            {
                Expected("NO");
                Expected("LOCK");
            }
        }

        InvalidKey(ref isConditional);

        if (isConditional)
            Expected("END-REWRITE");
    }

    private static void RESUME()
    {
        Expected("RESUME");
        Optional("AT");
        if (CurrentEquals("NEXT"))
        {
            Expected("NEXT");
            Expected("STATEMENT");
        }
        else
        {
            Identifier();
        }
    }

    private static void ROLLBACK()
    {
        Expected("ROLLBACK");
    }

    private static void SEARCH()
    {
        Expected("SEARCH");
        if (!CurrentEquals("ALL"))
        {
            Identifier();
            if (CurrentEquals("VARYING"))
            {
                Expected("VARYING");
                Identifier();
            }

            if (CurrentEquals("AT", "END"))
            {
                Optional("AT");
                Expected("END");
                ParseStatements(true);
            }

            if (!CurrentEquals("WHEN"))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    Expected at least one WHEN condition phrase.
                    """)
                .CloseError();
            }

            while (CurrentEquals("WHEN"))
            {
                Expected("WHEN");
                Condition();
                if (CurrentEquals("NEXT") && LookaheadEquals(1, "SENTENCE"))
                {
                    Error
                    .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                        Unsupported phrase: NEXT SENTENCE is an archaic feature.
                        """)
                    .WithSourceLine(Current(), $"""
                        The CONTINUE statement can be used to accomplish the same functionality.
                        """)
                    .CloseError();

                    AnchorPoint("WHEN", "END-SEARCH");
                }

                ParseStatements(true);
            }

            Expected("END-SEARCH");
            return;
        }

        Expected("ALL");
        Identifier();
        if (CurrentEquals("AT", "END"))
        {
            Optional("AT");
            Expected("END");
            ParseStatements(true);
        }

        Expected("WHEN");
        Identifier();
        if (CurrentEquals("IS", "EQUAL", "="))
        {
            Optional("IS");
            if (CurrentEquals("EQUAL"))
            {
                Expected("EQUAL");
                Optional("TO");
            }
            else
            {
                Expected("=");
            }
        }

        if (CurrentEquals(TokenType.Identifier, TokenType.String, TokenType.Numeric) && LookaheadEquals(1, TokenType.Symbol))
        {
            Arithmetic();
        }
        else if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, TokenType.Symbol))
        {
            Identifier();
        }
        else if (CurrentEquals(TokenType.Numeric))
        {
            Number();
        }
        else
        {
            String();
        }

        while (CurrentEquals("AND"))
        {
            Expected("AND");
            Identifier();
            if (CurrentEquals("IS", "EQUAL", "="))
            {
                Optional("IS");
                if (CurrentEquals("EQUAL"))
                {
                    Expected("EQUAL");
                    Optional("TO");
                }
                else
                {
                    Expected("=");
                }
            }

            if (CurrentEquals(TokenType.Identifier, TokenType.String, TokenType.Numeric) && LookaheadEquals(1, TokenType.Symbol))
            {
                Arithmetic();
            }
            else if (CurrentEquals(TokenType.Identifier) && !LookaheadEquals(1, TokenType.Symbol))
            {
                Identifier();
            }
            else if (CurrentEquals(TokenType.Numeric))
            {
                Number();
            }
            else
            {
                String();
            }

        }

        if (CurrentEquals("NEXT") && LookaheadEquals(1, "SENTENCE"))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unsupported phrase: NEXT SENTENCE is an archaic feature.
                """)
            .WithSourceLine(Current(), $"""
                The CONTINUE statement can be used to accomplish the same functionality.
                """)
            .CloseError();

            AnchorPoint("END-SEARCH");
        }

        ParseStatements(true);
        Expected("END-SEARCH");
    }

    private static void SEND()
    {
        bool isConditional = false;
        Expected("SEND");
        Optional("TO");

        if (LookaheadEquals(3, "RETURNING"))
        {
            if (CurrentEquals(TokenType.String))
            {
                String();
            }
            else
            {
                Identifier();
            }

            Expected("FROM");
            Identifier();
            Expected("RETURNING");
            Identifier();
        }
        else
        {
            Identifier();
            Expected("FROM");
            Identifier();

            if (CurrentEquals("RAISING"))
            {
                Expected("RAISING");
                if (CurrentEquals("LAST"))
                {
                    Expected("LAST");
                    Optional("EXCEPTION");
                }
                else
                {
                    Expected("EXCEPTION");
                    Identifier();
                }
            }
        }

        OnException(ref isConditional);

        if (isConditional) Expected("END-SEND");
    }

    private static void SET()
    {
        Expected("SET");

        if (CurrentEquals(TokenType.Identifier) || CurrentEquals("SIZE", "ADDRESS"))
        {
            // TODO: This needs to be fixed to lookup a qualified reference
            DataSignature dataItem = new();

            if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "UP", "DOWN", "TO"))
            {
                Identifier();
                if (CurrentEquals("UP"))
                {
                    Expected("UP");
                    Expected("BY");
                }
                else if (CurrentEquals("DOWN"))
                {
                    Expected("DOWN");
                    Expected("BY");
                }
                else
                {
                    Expected("TO");
                }

                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier(UsageType.Integer);
                }
                else
                {
                    Arithmetic();
                }
            }
            else if (CurrentEquals(TokenType.Identifier) && LookaheadEquals(1, "TO") && LookaheadEquals(2, "LOCALE"))
            {
                Identifier();
                Expected("TO");
                Expected("LOCALE");
                Choice("LC_ALL", "LOCALE");
            }
            else if (dataItem.UsageType == UsageType.MessageTag)
            {
                Identifier(UsageType.MessageTag);
                Expected("TO");
                if (CurrentEquals("NULL"))
                {
                    Expected("NULL");
                }
                else
                {
                    Identifier(UsageType.MessageTag);
                }
            }
            else if (dataItem.IsDynamicLength || CurrentEquals("SIZE"))
            {
                if (CurrentEquals("SIZE"))
                {
                    Expected("SIZE");
                    Optional("OF");
                }

                Identifier();
                Expected("TO");
                if (CurrentEquals(TokenType.Numeric))
                {
                    Number();
                }
                else
                {
                    Arithmetic();
                }
            }
            else if (dataItem.UsageType == UsageType.DataPointer || CurrentEquals("ADDRESS"))
            {
                bool hasAddress = false;

                if (CurrentEquals("ADDRESS"))
                {
                    hasAddress = true;
                    Expected("ADDRESS");
                    Optional("OF");
                    Identifier();
                }
                else
                {
                    Identifier(UsageType.DataPointer);
                }

                while (CurrentEquals(TokenType.Identifier) || CurrentEquals("ADDRESS"))
                {
                    if (CurrentEquals("ADDRESS"))
                    {
                        hasAddress = true;
                        Expected("ADDRESS");
                        Optional("OF");
                        Identifier();
                    }
                    else
                    {
                        Identifier(UsageType.DataPointer);
                    }
                }

                if (hasAddress || CurrentEquals("TO"))
                {
                    Expected("TO");
                    Identifier(UsageType.DataPointer);
                }
                else if (!hasAddress && CurrentEquals("UP", "DOWN"))
                {
                    Choice("UP", "DOWN");
                    Expected("BY");
                    Arithmetic();
                }
            }
            else if (dataItem.UsageType is UsageType.Integer or UsageType.Index)
            {

                Identifier();
                bool checkUsage = true;

                while (CurrentEquals(TokenType.Identifier))
                    Identifier(out checkUsage, UsageType.Index, UsageType.Integer);

                if (CurrentEquals("TO"))
                {
                    Expected("TO");
                    if (CurrentEquals(TokenType.Identifier))
                    {
                        Identifier(UsageType.Integer, UsageType.Index);
                    }
                    else
                    {
                        Arithmetic();
                    }
                }
                else if (checkUsage && CurrentEquals("UP", "DOWN"))
                {
                    Choice("UP", "DOWN");
                    Expected("BY");
                    Arithmetic();
                }

            }
        }
        else if (CurrentEquals("LAST"))
        {
            Expected("LAST");
            Expected("EXCEPTION");
            Expected("TO");
            Expected("OFF");
        }
        else if (CurrentEquals("LOCALE"))
        {
            Expected("LOCALE");
            if (CurrentEquals("USER-DEFAULT"))
            {
                Expected("USER-DEFAULT");
            }
            else
            {
                SetLocale();
            }

            Expected("TO");
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                Choice("USER-DEFAULT", "SYSTEM-DEFAULT");
            }
        }

    }

    private static void SORT()
    {
        Expected("SORT");
        Identifier();

        Optional("ON");
        if (CurrentEquals("ASCENDING"))
        {
            Expected("ASCENDING");
        }
        else
        {
            Expected("DESCENDING");
        }

        Optional("KEY");

        if (!CurrentEquals(TokenType.Identifier))
        {
            Error
            .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                Unexpected {Current().Type.Display(false)}.
                """)
            .WithSourceLine(Current(), $"""
                The ON ASCENDING / DESCENDING KEY phrase must only contain key names.
                """)
            .CloseError();
        }

        Identifier();
        while (Current().Type == TokenType.Identifier)
            Identifier();


        while (CurrentEquals("ON", "ASCENDING", "DESCENDING"))
        {
            Optional("ON");
            if (CurrentEquals("ASCENDING"))
            {
                Expected("ASCENDING");
            }
            else
            {
                Expected("DESCENDING");
            }

            Optional("KEY");

            if (!CurrentEquals(TokenType.Identifier))
            {
                Error
                .Build(ErrorType.Analyzer, ConsoleColor.Red, 5, $"""
                    Unexpected {Current().Type.Display(false)}.
                    """)
                .WithSourceLine(Current(), $"""
                    The ON ASCENDING / DESCENDING KEY phrase must only contain key names.
                    """)
                .CloseError();
            }

            Identifier();
            while (Current().Type == TokenType.Identifier)
                Identifier();
        }

        if (CurrentEquals("WITH", "DUPLICATES"))
        {
            Optional("WITH");
            Expected("DUPLICATES");
            Optional("IN");
            Optional("ORDER");
        }

        if (CurrentEquals("COLLATING", "SEQUENCE"))
        {
            Optional("COLLATING");
            Expected("SEQUENCE");
            if (CurrentEquals("IS") && LookaheadEquals(1, TokenType.Identifier) || CurrentEquals(TokenType.Identifier))
            {
                Optional("IS");
                Identifier();

                if (CurrentEquals(TokenType.Identifier)) Identifier();
            }
            else
            {
                if (!CurrentEquals("FOR", "ALPHANUMERIC", "NATIONAL"))
                {
                    ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
                    The COLLATING SEQUENCE clause must contain at least 1 alphabet name (max of 2 alphabet names) or at least one FOR ALPHANUMERIC and FOR NATIONAL clauses.
                    """);
                    ErrorHandler.Analyzer.PrettyError(FileName, Current());

                    CombinedAnchorPoint(TokenContext.IsStatement, "USING");
                }

                ForAlphanumericForNational();
            }
        }

        if (CurrentEquals("INPUT"))
        {
            Expected("INPUT");
            Expected("PROCEDURE");
            Optional("IS");
            Identifier();

            if (CurrentEquals("THROUGH", "THRU"))
            {
                Choice("THROUGH", "THRU");
                Identifier();
            }
        }
        else
        {
            Expected("USING");
            Identifier();
            while (Current().Type == TokenType.Identifier)
                Identifier();
        }

        if (CurrentEquals("OUTPUT"))
        {
            Expected("OUTPUT");
            Expected("PROCEDURE");
            Optional("IS");
            Identifier();

            if (CurrentEquals("THROUGH", "THRU"))
            {
                Choice("THROUGH", "THRU");
                Identifier();
            }
        }
        else
        {
            Expected("GIVING");
            Identifier();
            while (Current().Type == TokenType.Identifier)
                Identifier();
        }
    }

    private static void START()
    {
        bool isConditional = false;

        Expected("START");
        Identifier();

        if (CurrentEquals("FIRST"))
        {
            Expected("FIRST");
        }
        else if (CurrentEquals("LAST"))
        {
            Expected("LAST");
        }
        else if (CurrentEquals("KEY"))
        {
            Expected("KEY");
            StartRelationalOperator();
            Identifier();

            if (CurrentEquals("WITH", "LENGTH"))
            {
                Optional("WITH");
                Expected("LENGTH");
                Arithmetic(".");
            }

        }

        InvalidKey(ref isConditional);

        if (isConditional) Expected("END-START");
    }

    private static void STOP()
    {
        Expected("STOP");
        Expected("RUN");
        if (CurrentEquals("WITH") || CurrentEquals("NORMAL") || CurrentEquals("ERROR"))
        {
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
        }
    }

    private static void STRING()
    {
        bool isConditional = false;

        Expected("STRING");
        if (CurrentEquals(TokenType.Identifier)) Identifier();

        else String();

        while (CurrentEquals(TokenType.Identifier, TokenType.String))
        {
            if (CurrentEquals(TokenType.Identifier)) Identifier();

            else String();
        }

        Expected("DELIMITED");
        Optional("BY");
        if (CurrentEquals(TokenType.Identifier)) Identifier();

        else if (CurrentEquals("SIZE")) Expected("SIZE");

        else String();

        while (CurrentEquals(TokenType.Identifier, TokenType.String))
        {
            if (CurrentEquals(TokenType.Identifier)) Identifier();

            else String();

            while (CurrentEquals(TokenType.Identifier, TokenType.String))
            {
                if (CurrentEquals(TokenType.Identifier)) Identifier();

                else String();
            }

            Expected("DELIMITED");
            Optional("BY");
            if (CurrentEquals(TokenType.Identifier)) Identifier();

            else if (CurrentEquals("SIZE")) Expected("SIZE");

            else String();
        }

        Expected("INTO");
        Identifier();

        if (CurrentEquals("WITH", "POINTER"))
        {
            Optional("WITH");
            Expected("POINTER");
            Identifier();
        }

        OnOverflow(ref isConditional);

        if (isConditional) Expected("END-STRING");
    }

    private static void SUPPRESS()
    {
        Expected("SUPPRESS");
        Optional("PRINTING");
    }

    private static void TERMINATE()
    {
        Expected("TERMINATE");
        if (Current().Type != TokenType.Identifier)
        {
            ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
            The TERMINATE statement must only contain report entry identifiers defined in the report section.
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, Current());
        }
        Identifier();
        while (Current().Type == TokenType.Identifier)
            Identifier();

        if (!CurrentEquals("."))
        {
            ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
            The TERMINATE statement must only contain report entry identifiers defined in the report section.
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, Current());
        }
    }

    private static void UNLOCK()
    {
        Expected("UNLOCK");
        Identifier();
        Choice("RECORD", "RECORDS");
    }

    private static void UNSTRING()
    {
        bool isConditional = false;

        Expected("UNSTRING");
        Identifier();

        if (CurrentEquals("DELIMITED"))
        {
            Expected("DELIMITED");
            Optional("BY");
            if (CurrentEquals("ALL")) Expected("ALL");

            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                String();
            }

            while (CurrentEquals("OR"))
            {
                Expected("OR");
                if (CurrentEquals("ALL")) Expected("ALL");

                if (CurrentEquals(TokenType.Identifier))
                {
                    Identifier();
                }
                else
                {
                    String();
                }
            }
        }

        Expected("INTO");
        Identifier();

        if (CurrentEquals("DELIMITER"))
        {
            Expected("DELIMITER");
            Optional("IN");
            Identifier();
        }
        if (CurrentEquals("COUNT"))
        {
            Expected("COUNT");
            Optional("IN");
            Identifier();
        }

        while (CurrentEquals(TokenType.Identifier))
        {
            Identifier();

            if (CurrentEquals("DELIMITER"))
            {
                Expected("DELIMITER");
                Optional("IN");
                Identifier();
            }
            if (CurrentEquals("COUNT"))
            {
                Expected("COUNT");
                Optional("IN");
                Identifier();
            }
        }

        if (CurrentEquals("WITH", "POINTER"))
        {
            Optional("WITH");
            Expected("POINTER");
            Identifier();
        }

        if (CurrentEquals("TALLYING"))
        {
            Expected("TALLYING");
            Optional("IN");
            Identifier();
        }

        OnOverflow(ref isConditional);

        if (isConditional) Expected("END-UNSTRING");
    }

    private static void VALIDATE()
    {
        Expected("VALIDATE");
        if (Current().Type != TokenType.Identifier)
        {
            ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
            The VALIDATE statement must only contain data item identifiers.
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, Current());
        }
        Identifier();
        while (Current().Type == TokenType.Identifier)
            Identifier();

        if (!CurrentEquals("."))
        {
            ErrorHandler.Analyzer.Report(FileName, Current(), ErrorType.General, """
            The VALIDATE statement must only contain data item identifiers.
            """);
            ErrorHandler.Analyzer.PrettyError(FileName, Current());
        }
    }

    private static void WRITE()
    {
        bool isSequential = false;
        bool isConditional = false;

        Expected("WRITE");
        if (CurrentEquals("FILE"))
        {
            Expected("FILE");
            Identifier();
        }
        else
        {
            Identifier();
        }

        if (CurrentEquals("FROM"))
        {
            if (CurrentEquals(TokenType.Identifier))
            {
                Identifier();
            }
            else
            {
                String();
            }
        }

        if (CurrentEquals("BEFORE", "AFTER"))
        {
            isSequential = true;

            WriteBeforeAfter();
            Optional("ADVANCING");
            if (CurrentEquals("PAGE"))
            {
                Expected("PAGE");
                // Missing mnemonic-name handling
            }
            else if (CurrentEquals(TokenType.Identifier, TokenType.Numeric))
            {
                if (CurrentEquals(TokenType.Identifier)) Identifier();

                else Number();

                if (CurrentEquals("LINE", "LINES"))
                {
                    Expected(Current().Value);
                }
            }
        }

        RetryPhrase();

        if (CurrentEquals("WITH", "LOCK"))
        {
            Optional("WITH");
            Expected("LOCK");
        }
        else if (CurrentEquals("WITH", "NO"))
        {
            Optional("WITH");
            Expected("NO");
            Expected("LOCK");
        }

        if (isSequential && CurrentEquals("AT", "NOT", "END-OF-PAGE", "EOP"))
        {
            AtEndOfPage(ref isConditional);
        }
        else if (!isSequential && CurrentEquals("INVALID", "NOT"))
        {
            InvalidKey(ref isConditional);
        }

        if (isConditional) Expected("END-WRITE");
    }

    private static void PARAGRAPH()
    {
        Identifier(Current());
    }
}
