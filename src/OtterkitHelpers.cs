using System.Text;
using System.Text.RegularExpressions;

namespace Otterkit;

public static class Helpers
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

    public static readonly Dictionary<string, int> BooleanPrecedence = new()
    {
        ["("] = 0,
        ["NOT"] = 1,
        ["AND"] = 2,
        ["OR"] = 2,
        ["XOR"] = 2,
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
            if (token.type is TokenType.Numeric or TokenType.Identifier or TokenType.String)
            {
                output.Add(token);
            }
            else if (token.value.Equals("("))
            {
                stack.Push(token);
            }
            else if (token.value.Equals(")"))
            {
                while (!stack.Peek().value.Equals("("))
                {
                    output.Add(stack.Pop());
                }

                stack.Pop();
            }
            else if (precedence.ContainsKey(token.value))
            {
                var isExponentiation = token.value == "**";

                if (isArithmetic)
                while (stack.Count > 0 && ((precedence[stack.Peek().value] > precedence[token.value] && !isExponentiation) || (precedence[stack.Peek().value] >= precedence[token.value] && !isExponentiation && stack.Peek().value == "**")))
                {
                    output.Add(stack.Pop());
                }

                if (!isArithmetic)
                while (stack.Count > 0 && precedence[stack.Peek().value] >= precedence[token.value])
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
        Stack<string> stack = new();

        foreach (Token token in postfix)
        {
            if (token.type == TokenType.Numeric || token.type == TokenType.Identifier || token.type == TokenType.String)
            {
                stack.Push(token.value);
            }

            else if (precedence.ContainsKey(token.value))
            {
                string right = stack.Pop();
                string left = stack.Pop();
                stack.Push($"({left} {token.value} {right})");
            }
        }

        return stack.Pop();
    }

    public static bool IsBalanced(List<Token> tokens)
    {
        Stack<Token> stack = new();

        foreach (Token token in tokens)
        {
            if (token.value.Equals("("))
            {
                stack.Push(token);
            }
            else if (token.value.Equals(")"))
            {
                if (stack.Count == 0 || !stack.Pop().value.Equals("("))
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
            if (token.type is TokenType.Numeric or TokenType.Identifier or TokenType.String)
            {
                stack.Push(token);
            }
            else if (precedence.ContainsKey(token.value))
            {
                var isUnary = token.value == "NOT";

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

        error = new Token("", TokenType.EOF, -1, -1);
        return true;
    }

}