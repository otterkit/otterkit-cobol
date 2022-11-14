using System.Text;

namespace OtterkitLibrary;

public static class Helpers
{
    public static string ToPostfix(ReadOnlySpan<char> expression)
    {
        StringBuilder postfix = new();
        Stack<char> stack = new();
        int index = 0;

        foreach (char current in expression)
        {
            bool isOperand =
                char.IsLetterOrDigit(current) 
                || current == '.' 
                || (current == '-' 
                    && char.IsLetterOrDigit(expression[index + 1])
                    );
                    
            bool isOperator = 
                current != '(' && current != ')' 
                && !isOperand && !(current == ' ');

            if (isOperand)
                postfix.Append(current);

            if (current == '(')
                stack.Push(current);

            if (current == ')')
            {
                bool PeekIsNotChar(char character) =>
                    stack.Count > 0 && stack.Peek() != character;

                while (PeekIsNotChar('('))
                    postfix.Append(" " + stack.Pop());

                if (PeekIsNotChar('('))
                    throw new ArithmeticException("Invalid Infix Arithmetic Expression");

                if (!PeekIsNotChar('('))
                    stack.Pop();
            }

            if (isOperator)
            {
                int Precedence(char op) => op switch
                {
                        '+' => 1,
                        '-' => 1,
                        '*' => 2,
                        '/' => 2,
                        '^' => 3,
                        _ => 0
                };

                bool IsLowerPrecedence() =>
                    stack.Count > 0 && Precedence(current) <= Precedence(stack.Peek());

                while (IsLowerPrecedence())
                    postfix.Append(" " + stack.Pop());


                postfix.Append(' ');
                stack.Push(current);
            }
            index++;
        }

        while (stack.Count > 0)
            postfix.Append(" " + stack.Pop());

        return postfix.ToString();
    }
}