using static Otterkit.Types.TokenHandling;
using Otterkit.Analyzers;
using Otterkit.Types;
using System.Text;

namespace Otterkit.CodeGenerators;

public readonly struct StatementBuilder
{
    private readonly ProgramBuilder Builder;
    private readonly StringBuilder Compiled = new();


    public StatementBuilder(ProgramBuilder builder)
    {
        Builder = builder;
    }

    public void ExportStatement()
    {
        Builder.Append(Compiled);
    }

    public void BuildStatement()
    {
        Statement();
    }

    private void Statement()
    {
        if (CurrentEquals("DISPLAY"))
        {
            DISPLAY();
        }
        else if (CurrentEquals("CALL"))
        {
            CALL();
        }
        else if (CurrentEquals("ACCEPT"))
        {
            ACCEPT();
        }
        else if (CurrentEquals("STOP"))
        {
            STOP();
        }
        else if (CurrentEquals("IF"))
        {
            IF();
        }
    }

    private void DISPLAY()
    {
        Compiled.Append("Statement.DISPLAY(");

        Continue(1);

        while (Current().Type is TokenType.Identifier or TokenType.Numeric or TokenType.String)
        {
            string identifier;
            if (Current().Type == TokenType.Identifier)
            {
                identifier = FormatIdentifier(Current().Value);
                Compiled.Append($"{identifier}.Bytes, ");
            }

            if (Current().Type == TokenType.Numeric)
            {
                Compiled.Append($"\"{Current().Value}\"u8, ");
            }

            if (Current().Type == TokenType.String)
            {
                Compiled.Append($"{Current().Value}u8, ");
            }

            Continue(1);
        }

        if (CurrentEquals("UPON"))
        {
            Continue(1);
            if (CurrentEquals("STANDARD-OUTPUT"))
                Compiled.Append($"\"{Current().Value}\", ");

            if (CurrentEquals("STANDARD-ERROR"))
                Compiled.Append($"\"{Current().Value}\", ");

            Continue(1);
        }
        else if (!CurrentEquals("UPON"))
        {
            Compiled.Append("\"\", ");
        }

        if (CurrentEquals("WITH") || CurrentEquals("NO"))
            Compiled.Append("false);");

        if (!CurrentEquals("WITH") && !CurrentEquals("NO"))
            Compiled.Append("true);");
        ExportStatement();
    }

    private void CALL()
    {
        Continue(1);
        string ProgramName = $"{FormatIdentifier(Current().Value[1..^1])}";

        Compiled.Append($"{ProgramName} {ProgramName} = new();");
        Compiled.Append("Statement.CALL(");
        Compiled.Append($"() => {ProgramName}.Procedure());");

        ExportStatement();
    }

    private void ACCEPT()
    {
        Compiled.Append("Statement.ACCEPT(");
        // Statements.ACCEPT(dataItem, from, format)
        Continue(1);
        Compiled.Append($"{FormatIdentifier(Current().Value)}, ");
        Continue(1);

        if (!CurrentEquals("FROM"))
            Compiled.Append("\"STANDARD-INPUT\");");

        if (CurrentEquals("FROM"))
        {
            Continue(1);

            switch (Current().Value.ToUpperInvariant())
            {
                case "STANDARD-INPUT":
                case "COMMAND-LINE":
                    Compiled.Append($"\"{Current().Value}\");");
                    break;

                case "DATE":
                    Compiled.Append($"\"{Current().Value}\"");
                    if (PeekEquals(1, "YYYYMMDD"))
                        Compiled.Append($", \"{Peek(1).Value}\");");

                    if (!PeekEquals(1, "YYYYMMDD"))
                        Compiled.Append(");");
                    break;

                case "DAY":
                    Compiled.Append($"\"{Current().Value}\"");
                    if (PeekEquals(1, "YYYYDDD"))
                        Compiled.Append($", \"{Peek(1).Value}\");");

                    if (!PeekEquals(1, "YYYYDDD"))
                        Compiled.Append(");");
                    break;

                case "DAY-OF-WEEK":
                    Compiled.Append($"\"{Current().Value}\");");
                    break;

                case "TIME":
                    Compiled.Append($"\"{Current().Value}\");");
                    break;
            }
        }

        ExportStatement();
    }

    private void IF()
    {
        List<Token> expression = new();
        Continue(1);
        while (!CurrentEquals("THEN") && !CurrentEquals(TokenContext.IsStatement))
        {
            expression.Add(Current());
            Continue(1);
        }

        expression = Expressions.ShuntingYard(expression, Expressions.ConditionalPrecedence);

        foreach (var item in expression)
        {
            Console.WriteLine(item);
        }

        var compiledExpression = Expressions.PostfixToCSharpInfix(expression, Expressions.ConditionalPrecedence);

        Compiled.Append($"if ({compiledExpression}) {{");

        ExportStatement();
    }

    private void STOP()
    {
        // Statement.STOP();
        // Statement.STOP(error, status);
        Compiled.Append("Statement.STOP(");
        Continue(2);

        if (CurrentEquals("."))
        {
            Compiled.Append(");");
            ExportStatement();
            return;
        }

        if (CurrentEquals("WITH"))
            Continue(1);

        if (CurrentEquals("NORMAL"))
            Compiled.Append("false, ");

        if (CurrentEquals("ERROR"))
            Compiled.Append("true, ");

        Continue(1);
        if (CurrentEquals("."))
        {
            Compiled.Append("\"0\");");
            ExportStatement();
            return;
        }

        if (CurrentEquals("STATUS"))
        {
            Continue(1);
        }

        switch (Current().Type)
        {
            case TokenType.Identifier:
                Compiled.Append($"{FormatIdentifier(Current().Value)}.Display);");
                break;
            case TokenType.Numeric:
                Compiled.Append($"\"{Current().Value}\");");
                break;
            case TokenType.String:
                Compiled.Append($"{Current().Value});");
                break;
        }
        ExportStatement();
    }

    // Statement builder helper methods.
    private static string FormatIdentifier(string Identifier)
    {
        string FormattedIdentifier = Identifier;
        FormattedIdentifier = "_" + FormattedIdentifier.Replace("-", "_");
        return FormattedIdentifier;
    }
}
