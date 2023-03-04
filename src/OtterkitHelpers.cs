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
        var stack = new Stack<string>();

        foreach (var token in postfix)
        {
            var isUnary = token.value == "NOT";

            if (token.type is TokenType.Numeric or TokenType.Identifier or TokenType.String)
            {
                stack.Push(token.value);
            }

            else if (precedence.ContainsKey(token.value) && isUnary)
            {
                var right = stack.Pop();
                stack.Push($"({token.value} {right})");
            }

            else if (precedence.ContainsKey(token.value) && !isUnary)
            {
                var right = stack.Pop();
                var left = stack.Pop();
                stack.Push($"({left} {token.value} {right})");
            }
        }

        return stack.Pop();
    }

    public static string PostfixToCSharpInfix(List<Token> postfix, Dictionary<string, int> precedence)
    {
        var stack = new Stack<string>();

        foreach (var token in postfix)
        {
            var isUnary = token.value == "NOT";

            if (token.type is TokenType.Numeric or TokenType.Identifier or TokenType.String)
            {
                stack.Push(token.value);
            }

            else if (precedence.ContainsKey(token.value) && isUnary)
            {
                var right = stack.Pop();
                stack.Push($"!({right})");
            }

            else if (precedence.ContainsKey(token.value) && !isUnary)
            {
                var right = stack.Pop();
                var left = stack.Pop();

                switch (token.value)
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
                        stack.Push($"({left} {token.value} {right})"); break;
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

        error = new Token("NoError", TokenType.EOF, -1, -1);
        return true;
    }

    public static int ParsePictureString(ReadOnlySpan<char> picture, out HashSet<char> set)
    {
        var hashSet = new HashSet<char>();
        var dataSize = 0;

        for (var index = 0; index < picture.Length; index++)
        {
            var character = picture[index];

            if (character is 'B' or 'b' or '0' or '/') { }

            if (character == '(')
            {
                var start = index;

                while (picture[index] != ')') index++;

                var end = index;

                var count = int.Parse(picture.Slice(start + 1, end - start - 1));

                dataSize += count - 1;

                continue;
            }

            hashSet.Add(picture[index]);

            dataSize++;
        }
        
        set = hashSet;
        return dataSize;
    }

}