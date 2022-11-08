using System.Runtime.InteropServices;
using System.Numerics;
namespace OtterkitLibrary;

/// <summary>
/// Otterkit Decimal128 Struct
/// <para>IEEE 754 128-bit Decimal Floating Point Number.</para>
/// <para>This struct calls native C code from libmpdec to perform Decimal128 operations</para>
/// </summary>
public struct Decimal128:
    IMinMaxValue<Decimal128>,
    IEquatable<Decimal128>,
    IComparable<Decimal128>
{
    public string Value { get; set; }

    private static readonly Decimal128 maxValue = "9999999999999999999999999999999999E+6111";
    private static readonly Decimal128 minValue = "-9999999999999999999999999999999999E+6111";

    public static Decimal128 MaxValue => maxValue;

    public static Decimal128 MinValue => minValue;

    public static Decimal128 One => new Decimal128("1");

    public static Decimal128 Zero => new Decimal128("0");

    static Decimal128()
    {
        Decimal128 WarmUp = new Decimal128("5") + new Decimal128("5");
    }

    public Decimal128(string value)
    {
        this.Value = value;
    }

    public Decimal128(double value)
    {
        this.Value = value.ToString();
    }

    public Decimal128(decimal value)
    {
        this.Value = value.ToString();
    }

    public Decimal128(int value)
    {
        this.Value = value.ToString();
    }

    public Decimal128(long value)
    {
        this.Value = value.ToString();
    }

    public Decimal128(Int128 value)
    {
        this.Value = value.ToString();
    }

    public static implicit operator Decimal128(string value)
    {
        return new Decimal128(value);
    }

    public static implicit operator Decimal128(double value)
    {
        return new Decimal128(value);
    }

    public static implicit operator Decimal128(decimal value)
    {
        return new Decimal128(value);
    }

    public static implicit operator Decimal128(int value)
    {
        return new Decimal128(value);
    }

    public static implicit operator Decimal128(long value)
    {
        return new Decimal128(value);
    }

    public static implicit operator Decimal128(Int128 value)
    {
        return new Decimal128(value);
    }

    public static Decimal128 Pow(string value, string exponent)
    {
        return Decimal128Pow(value, exponent);
    }

    public static Decimal128 Sqrt(string value)
    {
        return Decimal128Sqrt(value);
    }

    public static Decimal128 Exp(string value)
    {
        return Decimal128Exp(value);
    }

    public static Decimal128 Abs(Decimal128 argument)
    {
        return Decimal128Abs(argument.Value);
    }

    public static Decimal128 Max(Decimal128 left, Decimal128 right)
    {
        return Decimal128Max(left.Value, right.Value);
    }

    public static Decimal128 Min(Decimal128 left, Decimal128 right)
    {
        return Decimal128Min(left.Value, right.Value);
    }

    public static Decimal128 NaturalLog(string value)
    {
        return Decimal128Ln(value);
    }

    public static Decimal128 Log10(string value)
    {
        return Decimal128Log10(value);
    }

    public static Decimal128 operator +(Decimal128 left, Decimal128 right)
    {
        return Decimal128Add(left.Value, right.Value);
    }

    public static Decimal128 operator +(Decimal128 argument)
    {
        return Decimal128Plus(argument.Value);
    }

    public static Decimal128 operator ++(Decimal128 argument)
    {
        return Decimal128Add(argument.Value, "1");
    }

    public static Decimal128 operator -(Decimal128 left, Decimal128 right)
    {
        return Decimal128Sub(left.Value, right.Value);
    }

    public static Decimal128 operator -(Decimal128 value)
    {
        return Decimal128Minus(value.Value);
    }

    public static Decimal128 operator --(Decimal128 argument)
    {
        return Decimal128Sub(argument.Value, "1");
    }

    public static Decimal128 operator *(Decimal128 left, Decimal128 right)
    {
        return Decimal128Mul(left.Value, right.Value);
    }

    public static Decimal128 operator /(Decimal128 left, Decimal128 right)
    {
        return Decimal128Div(left.Value, right.Value);
    }

    public static Decimal128 operator %(Decimal128 left, Decimal128 right)
    {
        return Decimal128Rem(left.Value, right.Value);
    }

    public static bool operator ==(Decimal128 left, Decimal128 right)
    {
        string result = Decimal128Compare(left.Value, right.Value);
        return result == "0" ? true : false;
    }

    public static bool operator !=(Decimal128 left, Decimal128 right)
    {
        string result = Decimal128Compare(left.Value, right.Value);
        return result != "0" ? true : false;
    }

    public static bool operator >(Decimal128 left, Decimal128 right)
    {
        string result = Decimal128Compare(left.Value, right.Value);
        return result == "1" ? true : false;
    }
    
    public static bool operator <(Decimal128 left, Decimal128 right)
    {
        string result = Decimal128Compare(left.Value, right.Value);
        return result == "-1" ? true : false;
    }

    public static bool operator >=(Decimal128 left, Decimal128 right)
    {
        string result = Decimal128Compare(left.Value, right.Value);
        return result == "0" || result == "1" ? true : false;
    }

    public static bool operator <=(Decimal128 left, Decimal128 right)
    {
        string result = Decimal128Compare(left.Value, right.Value);
        return result == "-1" || result == "0" ? true : false;
    }

    public static Decimal128 operator <<(Decimal128 left, int right)
    {
        Decimal128 shiftAmount = right;
        return Decimal128Shift(left.Value, Decimal128Minus(shiftAmount.Value));
    }

    public static Decimal128 operator >>(Decimal128 left, int right)
    {
        Decimal128 shiftAmount = right;
        return Decimal128Shift(left.Value, Decimal128Minus(shiftAmount.Value));
    }

    public static Decimal128 operator &(Decimal128 left, Decimal128 right)
    {
        return Decimal128And(left.Value, right.Value);
    }

    public static Decimal128 operator |(Decimal128 left, Decimal128 right)
    {
        return Decimal128Or(left.Value, right.Value);
    }

    public static Decimal128 operator ^(Decimal128 left, Decimal128 right)
    {
        return Decimal128And(left.Value, right.Value);
    }
    
    public int CompareTo(object? obj)
    {
        if (obj == null || GetType() != obj.GetType())
        {
            throw new ArgumentException();
        }
        return CompareTo((Decimal128) obj);
    }

    public int CompareTo(Decimal128 other)
    {
        int value = int.Parse(Decimal128Compare(this.Value, other.Value));
        return value;
    }

    public bool Equals(Decimal128 other)
    {
        return other.Value == Value;
    }

    public override bool Equals(object? obj)
    {
        if (obj == null || GetType() != obj.GetType())
        {
            return false;
        }
        
        return obj is Decimal128 && Equals((Decimal128) obj);
    }

    public override int GetHashCode()
    {
        return Value.GetHashCode();
    }

    public override string ToString()
    {
        return Value;
    }

    // Import native C code from libmpdec
    [DllImport("Decimal128")]
    static extern string Decimal128Pow(string value, string exponent);

    [DllImport("Decimal128")]
    static extern string Decimal128Exp(string value);

    [DllImport("Decimal128")]
    static extern string Decimal128Sqrt(string value);

    [DllImport("Decimal128")]
    static extern string Decimal128Ln(string value);

    [DllImport("Decimal128")]
    static extern string Decimal128Log10(string value);

    [DllImport("Decimal128")]
    static extern string Decimal128Abs(string value);

    [DllImport("Decimal128")]
    static extern string Decimal128Plus(string value);

    [DllImport("Decimal128")]
    static extern string Decimal128Minus(string value);

    [DllImport("Decimal128")]
    static extern string Decimal128Add(string left, string right);
   
    [DllImport("Decimal128")]
    static extern string Decimal128Sub(string value, string exponent);

    [DllImport("Decimal128")]
    static extern string Decimal128Div(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Rem(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Mul(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Compare(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Max(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Min(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Shift(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Rotate(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128And(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Or(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Xor(string left, string right);

    [DllImport("Decimal128")]
    static extern string Decimal128Invert(string value);
}

