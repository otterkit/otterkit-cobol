using System.Runtime.InteropServices;
namespace OtterkitLibrary;

/// <summary>
/// Otterkit DecimalHolder ref struct
/// <para>This ref struct holds an IEEE 754 128-bit Decimal Floating Point Number</para>
/// <para>Decimal operations are done by calling DecimalMath static methods</para>
/// </summary>
public readonly ref struct DecimalHolder
{
    public readonly ReadOnlySpan<byte> Bytes;
    public readonly bool IsNegative = false;

    public DecimalHolder(ReadOnlySpan<byte> bytes)
    {
        this.Bytes = bytes;
        if (bytes[0] == 45)
            IsNegative = true;
    }

    public static implicit operator DecimalHolder(ReadOnlySpan<byte> bytes)
    {
        return new DecimalHolder(bytes);
    }

    public static DecimalHolder operator +(DecimalHolder left, DecimalHolder right)
    {
        return new(DecimalMath.Add(left.Bytes, right.Bytes));
    }

    public static DecimalHolder operator +(DecimalHolder number)
    {
        return DecimalMath.Plus(number);
    }

    public static DecimalHolder operator ++(DecimalHolder number)
    {
        return new(DecimalMath.Add(number.Bytes, "1"u8));
    }

    public static DecimalHolder operator -(DecimalHolder left, DecimalHolder right)
    {
        return new(DecimalMath.Subtract(left.Bytes, right.Bytes));
    }

    public static DecimalHolder operator -(DecimalHolder number)
    {
        return DecimalMath.Minus(number);
    }

    public static DecimalHolder operator --(DecimalHolder number)
    {
        return new(DecimalMath.Subtract(number.Bytes, "1"u8));
    }

    public static DecimalHolder operator *(DecimalHolder left, DecimalHolder right)
    {
        return new(DecimalMath.Multiply(left.Bytes, right.Bytes));
    }

    public static DecimalHolder operator /(DecimalHolder left, DecimalHolder right)
    {
        return new(DecimalMath.Divide(left.Bytes, right.Bytes));
    }

    public static DecimalHolder operator %(DecimalHolder left, DecimalHolder right)
    {
        return DecimalMath.Rem(left, right);
    }

    public static bool operator ==(DecimalHolder left, DecimalHolder right)
    {
        DecimalHolder result = DecimalMath.Compare(left, right);
        return result.Bytes[0] == 48 ? true : false;
    }

    public static bool operator !=(DecimalHolder left, DecimalHolder right)
    {
        DecimalHolder result = DecimalMath.Compare(left, right);
        return result.Bytes[0] != 48 ? true : false;
    }

    public static bool operator >(DecimalHolder left, DecimalHolder right)
    {
        DecimalHolder result = DecimalMath.Compare(left, right);
        return result.Bytes[0] == 49 ? true : false;
    }

    public static bool operator <(DecimalHolder left, DecimalHolder right)
    {
        DecimalHolder result = DecimalMath.Compare(left, right);
        return result.Bytes[0] == 45 && result.Bytes[1] == 49 ? true : false;
    }

    public static bool operator >=(DecimalHolder left, DecimalHolder right)
    {
        DecimalHolder result = DecimalMath.Compare(left, right);
        return result.Bytes[0] == 48 || result.Bytes[0] == 49 ? true : false;
    }

    public static bool operator <=(DecimalHolder left, DecimalHolder right)
    {
        DecimalHolder result = DecimalMath.Compare(left, right);
        return (result.Bytes[0] == 45 && result.Bytes[1] == 49) || result.Bytes[0] == 48 ? true : false;
    }

    public override bool Equals(object? obj)
    {
        throw new System.NotImplementedException();
    }

    public override int GetHashCode()
    {
        throw new System.NotImplementedException();
    }

    public string Display() => System.Text.Encoding.UTF8.GetString(Bytes);
}

/// <summary>
/// Otterkit DecimalMath static class
/// <para>This static class contains methods to perform math operations on a DecimalHolder</para>
/// <para>Decimal operations are done by calling native C code from the mpdecimal library</para>
/// </summary>
public static class DecimalMath
{
    internal static unsafe ReadOnlySpan<byte> Arithmetic(ReadOnlySpan<byte> expression)
    {
        Span<byte> withNullTerminator = stackalloc byte[expression.Length + 1];

        expression.CopyTo(withNullTerminator);
        withNullTerminator[expression.Length] = 0;

        byte* result;

        fixed (byte* Pointer = withNullTerminator)
        {
            result = OtterkitArithmetic(Pointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }
        
        return new ReadOnlySpan<byte>(result, length - 1);
    }

    internal static unsafe ReadOnlySpan<byte> Add(ReadOnlySpan<byte> left, ReadOnlySpan<byte> right)
    {
        Span<byte> expression = stackalloc byte[left.Length + right.Length + 3];
        expression.Fill(32);

        left.CopyTo(expression);
        right.CopyTo(expression.Slice(left.Length + 1));
        expression[left.Length + right.Length + 2] = 43;

        ReadOnlySpan<byte> result = Arithmetic(expression);
        fixed (byte* Pointer = result)
        {
            return new ReadOnlySpan<byte>(Pointer, result.Length);
        }
    }

    internal static unsafe ReadOnlySpan<byte> Subtract(ReadOnlySpan<byte> left, ReadOnlySpan<byte> right)
    {
        Span<byte> expression = stackalloc byte[left.Length + right.Length + 3];
        expression.Fill(32);

        left.CopyTo(expression);
        right.CopyTo(expression.Slice(left.Length + 1));
        expression[left.Length + right.Length + 2] = 45;

        ReadOnlySpan<byte> result = Arithmetic(expression);
        fixed (byte* Pointer = result)
        {
            return new ReadOnlySpan<byte>(Pointer, result.Length);
        }
    }

    internal static unsafe ReadOnlySpan<byte> Multiply(ReadOnlySpan<byte> left, ReadOnlySpan<byte> right)
    {
        Span<byte> expression = stackalloc byte[left.Length + right.Length + 3];
        expression.Fill(32);

        left.CopyTo(expression);
        right.CopyTo(expression.Slice(left.Length + 1));
        expression[left.Length + right.Length + 2] = 42;

        ReadOnlySpan<byte> result = Arithmetic(expression);
        fixed (byte* Pointer = result)
        {
            return new ReadOnlySpan<byte>(Pointer, result.Length);
        }
    }

    internal static unsafe ReadOnlySpan<byte> Divide(ReadOnlySpan<byte> left, ReadOnlySpan<byte> right)
    {
        Span<byte> expression = stackalloc byte[left.Length + right.Length + 3];
        expression.Fill(32);

        left.CopyTo(expression);
        right.CopyTo(expression.Slice(left.Length + 1));
        expression[left.Length + right.Length + 2] = 47;

        ReadOnlySpan<byte> result = Arithmetic(expression);
        fixed (byte* Pointer = result)
        {
            return new ReadOnlySpan<byte>(Pointer, result.Length);
        }
    }

    public static unsafe DecimalHolder Pow(DecimalHolder left, DecimalHolder right)
    {
        Span<byte> expression = stackalloc byte[left.Bytes.Length + right.Bytes.Length + 3];
        expression.Fill(32);

        left.Bytes.CopyTo(expression);
        right.Bytes.CopyTo(expression.Slice(left.Bytes.Length + 1));
        expression[left.Bytes.Length + right.Bytes.Length + 2] = 94;

        ReadOnlySpan<byte> result = Arithmetic(expression);
        fixed (byte* Pointer = result)
        {
            return new ReadOnlySpan<byte>(Pointer, result.Length);
        }
    }

    public static unsafe DecimalHolder Exp(DecimalHolder exponent)
    {
        Span<byte> withNullTerminator = stackalloc byte[exponent.Bytes.Length + 1];

        exponent.Bytes.CopyTo(withNullTerminator);
        withNullTerminator[exponent.Bytes.Length] = 0;

        byte* result;

        fixed (byte* Pointer = withNullTerminator)
        {
            result = Decimal128Exp(Pointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Sqrt(DecimalHolder radicand)
    {
        Span<byte> withNullTerminator = stackalloc byte[radicand.Bytes.Length + 1];

        radicand.Bytes.CopyTo(withNullTerminator);
        withNullTerminator[radicand.Bytes.Length] = 0;

        byte* result;

        fixed (byte* Pointer = withNullTerminator)
        {
            result = Decimal128Sqrt(Pointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Ln(DecimalHolder argument)
    {
        Span<byte> withNullTerminator = stackalloc byte[argument.Bytes.Length + 1];

        argument.Bytes.CopyTo(withNullTerminator);
        withNullTerminator[argument.Bytes.Length] = 0;

        byte* result;

        fixed (byte* Pointer = withNullTerminator)
        {
            result = Decimal128Ln(Pointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Log10(DecimalHolder argument)
    {
        Span<byte> withNullTerminator = stackalloc byte[argument.Bytes.Length + 1];

        argument.Bytes.CopyTo(withNullTerminator);
        withNullTerminator[argument.Bytes.Length] = 0;

        byte* result;

        fixed (byte* Pointer = withNullTerminator)
        {
            result = Decimal128Log10(Pointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Abs(DecimalHolder number)
    {
        Span<byte> withNullTerminator = stackalloc byte[number.Bytes.Length + 1];

        number.Bytes.CopyTo(withNullTerminator);
        withNullTerminator[number.Bytes.Length] = 0;

        byte* result;

        fixed (byte* Pointer = withNullTerminator)
        {
            result = Decimal128Abs(Pointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Plus(DecimalHolder number)
    {
        Span<byte> withNullTerminator = stackalloc byte[number.Bytes.Length + 1];

        number.Bytes.CopyTo(withNullTerminator);
        withNullTerminator[number.Bytes.Length] = 0;

        byte* result;

        fixed (byte* Pointer = withNullTerminator)
        {
            result = Decimal128Plus(Pointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Minus(DecimalHolder number)
    {
        Span<byte> withNullTerminator = stackalloc byte[number.Bytes.Length + 1];

        number.Bytes.CopyTo(withNullTerminator);
        withNullTerminator[number.Bytes.Length] = 0;

        byte* result;

        fixed (byte* Pointer = withNullTerminator)
        {
            result = Decimal128Minus(Pointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Rem(DecimalHolder left, DecimalHolder right)
    {
        Span<byte> leftWithNullTerminator = stackalloc byte[left.Bytes.Length + 1];
        Span<byte> rightWithNullTerminator = stackalloc byte[right.Bytes.Length + 1];

        left.Bytes.CopyTo(leftWithNullTerminator);
        rightWithNullTerminator[left.Bytes.Length] = 0;

        right.Bytes.CopyTo(rightWithNullTerminator);
        rightWithNullTerminator[right.Bytes.Length] = 0;

        byte* result;

        fixed (byte* LeftPointer = leftWithNullTerminator, RightPointer = rightWithNullTerminator)
        {
            result = Decimal128Rem(LeftPointer, RightPointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Compare(DecimalHolder left, DecimalHolder right)
    {
        Span<byte> leftWithNullTerminator = stackalloc byte[left.Bytes.Length + 1];
        Span<byte> rightWithNullTerminator = stackalloc byte[right.Bytes.Length + 1];
    
        left.Bytes.CopyTo(leftWithNullTerminator);
        leftWithNullTerminator[left.Bytes.Length] = 0;

        right.Bytes.CopyTo(rightWithNullTerminator);
        rightWithNullTerminator[right.Bytes.Length] = 0;

        byte* result;

        fixed (byte* LeftPointer = leftWithNullTerminator, RightPointer = rightWithNullTerminator)
        {
            result = Decimal128Compare(LeftPointer, RightPointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Min(DecimalHolder left, DecimalHolder right)
    {
        Span<byte> leftWithNullTerminator = stackalloc byte[left.Bytes.Length + 1];
        Span<byte> rightWithNullTerminator = stackalloc byte[right.Bytes.Length + 1];

        left.Bytes.CopyTo(leftWithNullTerminator);
        rightWithNullTerminator[left.Bytes.Length] = 0;

        right.Bytes.CopyTo(rightWithNullTerminator);
        rightWithNullTerminator[right.Bytes.Length] = 0;

        byte* result;

        fixed (byte* LeftPointer = leftWithNullTerminator, RightPointer = rightWithNullTerminator)
        {
            result = Decimal128Min(LeftPointer, RightPointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Max(DecimalHolder left, DecimalHolder right)
    {
        Span<byte> leftWithNullTerminator = stackalloc byte[left.Bytes.Length + 1];
        Span<byte> rightWithNullTerminator = stackalloc byte[right.Bytes.Length + 1];

        left.Bytes.CopyTo(leftWithNullTerminator);
        rightWithNullTerminator[left.Bytes.Length] = 0;

        right.Bytes.CopyTo(rightWithNullTerminator);
        rightWithNullTerminator[right.Bytes.Length] = 0;

        byte* result;

        fixed (byte* LeftPointer = leftWithNullTerminator, RightPointer = rightWithNullTerminator)
        {
            result = Decimal128Max(LeftPointer, RightPointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Shift(DecimalHolder left, DecimalHolder right)
    {
        Span<byte> leftWithNullTerminator = stackalloc byte[left.Bytes.Length + 1];
        Span<byte> rightWithNullTerminator = stackalloc byte[right.Bytes.Length + 1];

        left.Bytes.CopyTo(leftWithNullTerminator);
        rightWithNullTerminator[left.Bytes.Length] = 0;

        right.Bytes.CopyTo(rightWithNullTerminator);
        rightWithNullTerminator[right.Bytes.Length] = 0;

        byte* result;

        fixed (byte* LeftPointer = leftWithNullTerminator, RightPointer = rightWithNullTerminator)
        {
            result = Decimal128Shift(LeftPointer, RightPointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder And(DecimalHolder left, DecimalHolder right)
    {
        Span<byte> leftWithNullTerminator = stackalloc byte[left.Bytes.Length + 1];
        Span<byte> rightWithNullTerminator = stackalloc byte[right.Bytes.Length + 1];

        left.Bytes.CopyTo(leftWithNullTerminator);
        rightWithNullTerminator[left.Bytes.Length] = 0;

        right.Bytes.CopyTo(rightWithNullTerminator);
        rightWithNullTerminator[right.Bytes.Length] = 0;

        byte* result;

        fixed (byte* LeftPointer = leftWithNullTerminator, RightPointer = rightWithNullTerminator)
        {
            result = Decimal128And(LeftPointer, RightPointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Or(DecimalHolder left, DecimalHolder right)
    {
        Span<byte> leftWithNullTerminator = stackalloc byte[left.Bytes.Length + 1];
        Span<byte> rightWithNullTerminator = stackalloc byte[right.Bytes.Length + 1];

        left.Bytes.CopyTo(leftWithNullTerminator);
        rightWithNullTerminator[left.Bytes.Length] = 0;

        right.Bytes.CopyTo(rightWithNullTerminator);
        rightWithNullTerminator[right.Bytes.Length] = 0;

        byte* result;

        fixed (byte* LeftPointer = leftWithNullTerminator, RightPointer = rightWithNullTerminator)
        {
            result = Decimal128Or(LeftPointer, RightPointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    public static unsafe DecimalHolder Xor(DecimalHolder left, DecimalHolder right)
    {
        Span<byte> leftWithNullTerminator = stackalloc byte[left.Bytes.Length + 1];
        Span<byte> rightWithNullTerminator = stackalloc byte[right.Bytes.Length + 1];

        left.Bytes.CopyTo(leftWithNullTerminator);
        rightWithNullTerminator[left.Bytes.Length] = 0;

        right.Bytes.CopyTo(rightWithNullTerminator);
        rightWithNullTerminator[right.Bytes.Length] = 0;

        byte* result;

        fixed (byte* LeftPointer = leftWithNullTerminator, RightPointer = rightWithNullTerminator)
        {
            result = Decimal128Xor(LeftPointer, RightPointer);
        }

        int length = 0;
        byte current = result[0];

        while (current != 0)
        {
            current = result[length];
            length++;
        }

        return new ReadOnlySpan<byte>(result, length - 1);
    }

    // Import native C code from libmpdec
    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* OtterkitArithmetic(byte* expression);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Exp(byte* exponent);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Sqrt(byte* radicand);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Ln(byte* argument);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Log10(byte* argument);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Abs(byte* number);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Plus(byte* number);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Minus(byte* number);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Rem(byte* left, byte* right);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Compare(byte* left, byte* right);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Min(byte* left, byte* right);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Max(byte* left, byte* right);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Shift(byte* left, byte* right);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128And(byte* left, byte* right);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Or(byte* left, byte* right);

    [DllImport("OtterkitMath/Decimal128")]
    static extern unsafe byte* Decimal128Xor(byte* left, byte* right);
}
