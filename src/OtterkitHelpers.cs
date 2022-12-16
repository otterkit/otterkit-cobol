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
        ["<"] = 2,
        [">"] = 2,
        ["<="] = 2,
        [">="] = 2,
        ["="] = 2,
        ["<>"] = 2,
        ["AND"] = 3,
        ["OR"] = 4,
        ["XOR"] = 4,
        [")"] = 5,
    };

    public static List<Token> ShuntingYard(List<Token> input, Dictionary<string, int> precedence)
    {
        List<Token> output = new();
        Stack<Token> stack = new();
        bool isArithmetic = precedence.ContainsKey("+");

        foreach (Token token in input)
        {
            if (token.type == TokenType.Numeric || token.type == TokenType.Identifier || token.type == TokenType.String)
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
                bool isExponentiation = token.value == "**";

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

    public static string PostfixToInfix(string postfix)
    {
        Stack<string> stack = new();

        foreach (var token in postfix.Split(' '))
        {
            if (Regex.IsMatch(token, @"^\d+$"))
            {
                stack.Push(token);
            }

            else if (Regex.IsMatch(token, @"^[+-/*]$") || token == "**")
            {
                string right = stack.Pop();
                string left = stack.Pop();
                stack.Push($"({left} {token} {right})");
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

    public static bool EvaluatePostfix(List<Token> expression, Dictionary<string, int> precedence)
    {
        Stack<int> stack = new();

        foreach (Token token in expression)
        {
            if (token.type == TokenType.Numeric || token.type == TokenType.Identifier || token.type == TokenType.String)
            {
                stack.Push(5);
            }
            else if (precedence.ContainsKey(token.value))
            {
                if (stack.Count < 2)
                {
                    return false;
                }

                int right = stack.Pop();
                int left = stack.Pop();
                stack.Push(5);
            }
            else
            {
                return false;
            }
        }

        if (stack.Count != 1)
        {
            return false;
        }

        return true;
    }
}