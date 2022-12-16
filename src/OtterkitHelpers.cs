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

    public static string ShuntingYard(List<Token> input, Dictionary<string, int> precedence)
    {
        List<Token> output = new();
        Stack<Token> stack = new();

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

                while (stack.Count > 0 && ((precedence[stack.Peek().value] > precedence[token.value] && !isExponentiation) || (precedence[stack.Peek().value] >= precedence[token.value] && !isExponentiation && stack.Peek().value == "**")))
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

        StringBuilder postfix = new();
        foreach (Token token in output)
        {
            postfix.Append($"{token.value} ");
        }
        return postfix.ToString();
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
}