using Otterkit.Types;

namespace Otterkit.Analyzers;

public static class Expressions
{
    public static readonly Dictionary<string, int> ArithmeticPrecedence = new()
    {
        ["("] = 0,
        ["+"] = 1,
        ["-"] = 1,
        ["*"] = 2,
        ["/"] = 2,
        ["**"] = 3,
        [")"] = 4,
    };

    public static readonly Dictionary<string, int> ConditionalPrecedence = new()
    {
        ["("] = 0,
        ["AND"] = 1,
        ["OR"] = 1,
        ["XOR"] = 1,
        ["EXCLUSIVE-OR"] = 1,
        ["NOT"] = 2,
        ["<"] = 3,
        [">"] = 3,
        ["NOT <"] = 3,
        ["NOT >"] = 3,
        ["<="] = 3,
        [">="] = 3,
        ["="] = 3,
        ["<>"] = 3,
        [")"] = 4,
    };

    public static List<Token> ShuntingYard(List<Token> input, Dictionary<string, int> precedence)
    {
        var output = new List<Token>();
        var stack = new Stack<Token>();
        var isArithmetic = precedence.ContainsKey("+");

        foreach (var token in input)
        {
            if (token.Type is TokenType.Numeric or TokenType.Identifier or TokenType.String)
            {
                output.Add(token);
            }
            else if (token.Value.Equals("("))
            {
                stack.Push(token);
            }
            else if (token.Value.Equals(")"))
            {
                while (!stack.Peek().Value.Equals("("))
                {
                    output.Add(stack.Pop());
                }

                stack.Pop();
            }
            else if (precedence.ContainsKey(token.Value))
            {
                var isExponentiation = token.Value == "**";

                if (isArithmetic)
                while (stack.Count > 0 && ((precedence[stack.Peek().Value] > precedence[token.Value] && !isExponentiation) || (precedence[stack.Peek().Value] >= precedence[token.Value] && !isExponentiation && stack.Peek().Value == "**")))
                {
                    output.Add(stack.Pop());
                }

                if (!isArithmetic)
                while (stack.Count > 0 && precedence[stack.Peek().Value] >= precedence[token.Value])
                {
                    output.Add(stack.Pop());
                }

                stack.Push(token);
            }
        }

        while (stack.Count > 0)
        {
            output.Add(stack.Pop());
        }

        return output;
    }

    public static string PostfixToInfix(List<Token> postfix, Dictionary<string, int> precedence)
    {
        var stack = new Stack<string>();

        foreach (var token in postfix)
        {
            var isUnary = token.Value == "NOT";

            if (token.Type is TokenType.Numeric or TokenType.Identifier or TokenType.String)
            {
                stack.Push(token.Value);
            }

            else if (precedence.ContainsKey(token.Value) && isUnary)
            {
                var right = stack.Pop();
                stack.Push($"({token.Value} {right})");
            }

            else if (precedence.ContainsKey(token.Value) && !isUnary)
            {
                var right = stack.Pop();
                var left = stack.Pop();
                stack.Push($"({left} {token.Value} {right})");
            }
        }

        return stack.Pop();
    }

    public static string PostfixToCSharpInfix(List<Token> postfix, Dictionary<string, int> precedence)
    {
        var stack = new Stack<string>();

        foreach (var token in postfix)
        {
            var isUnary = token.Value == "NOT";

            if (token.Type is TokenType.Numeric or TokenType.Identifier or TokenType.String)
            {
                stack.Push(token.Value);
            }

            else if (precedence.ContainsKey(token.Value) && isUnary)
            {
                var right = stack.Pop();
                stack.Push($"!({right})");
            }

            else if (precedence.ContainsKey(token.Value) && !isUnary)
            {
                var right = stack.Pop();
                var left = stack.Pop();

                switch (token.Value)
                {
                    case "NOT >":
                        stack.Push($"!({left} > {right})"); break;

                    case "NOT <":
                        stack.Push($"!({left} < {right})"); break;

                    case "<>":
                        stack.Push($"({left} != {right})"); break;

                    case "=":
                        stack.Push($"({left} == {right})"); break;

                    case "AND":
                        stack.Push($"({left} && {right})"); break;

                    case "OR":
                        stack.Push($"({left} || {right})"); break;

                    case "XOR":
                    case "EXCLUSIVE-OR":
                        stack.Push($"({left} ^ {right})"); break;

                    default:
                        stack.Push($"({left} {token.Value} {right})"); break;
                }

            }
        }

        return stack.Pop();
    }

    public static bool IsBalanced(List<Token> tokens)
    {
        Stack<Token> stack = new();

        foreach (Token token in tokens)
        {
            if (token.Value.Equals("("))
            {
                stack.Push(token);
            }
            else if (token.Value.Equals(")"))
            {
                if (stack.Count == 0 || !stack.Pop().Value.Equals("("))
                {
                    return false;
                }
            }
        }

        return stack.Count == 0;
    }

    public static bool EvaluatePostfix(List<Token> expression, Dictionary<string, int> precedence, out Token error)
    {
        Stack<Token> stack = new();

        foreach (var token in expression)
        {
            if (token.Type is TokenType.Numeric or TokenType.Identifier or TokenType.String)
            {
                stack.Push(token);
            }
            else if (precedence.ContainsKey(token.Value))
            {
                var isUnary = token.Value == "NOT";

                if (stack.Count < 1 && isUnary)
                {
                    error = stack.Pop();
                    return false;
                }

                if (stack.Count < 2 && !isUnary)
                {
                    error = stack.Pop();
                    return false;
                }

                if (isUnary)
                {
                    var unary = stack.Pop();
                    stack.Push(token);
                }

                if (!isUnary)
                {
                    var right = stack.Pop();
                    var left = stack.Pop();
                    stack.Push(token);
                }
            }
            else
            {
                error = token;
                return false;
            }
        }

        if (stack.Count != 1)
        {
            error = stack.Pop();
            return false;
        }

        error = new Token("NoError", TokenType.EOF, -1, -1);
        return true;
    }
}