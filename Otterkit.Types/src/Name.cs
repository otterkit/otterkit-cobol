using System.Diagnostics.CodeAnalysis;

namespace Otterkit.Types;

public readonly struct Name<TValue> : 
    IEquatable<Name<TValue>>
    where TValue : notnull
{
    internal readonly TValue Value;
    public readonly bool Exists;
    public readonly bool Unique;

    public static Name<TValue> Null => new();

    public Name(TValue value)
    {
        Value = value;
        Exists = true;
    }

    public Name(TValue value, bool unique)
    {
        Value = value;
        Exists = true;
        Unique = unique;
    }

    public void Deconstruct(out TValue? value, out bool exists)
    {
        value = Value;
        exists = Exists;
    }

    public bool TryUnwrap([NotNullWhen(true)] out TValue? value)
    {
        if (Exists)
        {
            value = Value;
            return true;
        }

        value = default;
        return false;
    }

    public TValue Unwrap()
    {
        if (Exists) return Value;

        throw new NullReferenceException("Instance of Name did not have a value");
    }

    public static implicit operator Name<TValue>(TValue? value)
    {
        if (value is null) return Null;

        return new(value);
    }

    public static explicit operator TValue(Name<TValue> option)
    {
        if (option.Exists) return option.Value;

        throw new NullReferenceException("Instance of Name did not have a value");
    }

    public static implicit operator bool(Name<TValue> option)
    {
        return option.Exists;
    }

    public static implicit operator Name<TValue>((TValue value, bool unique) tuple)
    {
        if (tuple.value is null) return Null;

        return new(tuple.value, tuple.unique);
    }

    public static bool operator ==(Name<TValue> left, Name<TValue> right)
    {
        return left.Equals(right);
    }

    public static bool operator !=(Name<TValue> left, Name<TValue> right)
    {
        return !left.Equals(right);
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        return obj is Name<TValue> value && Equals(value);
    }

    public bool Equals(Name<TValue> other)
    {
        if (Exists && other.Exists)
        {
            return EqualityComparer<TValue>.Default.Equals(Value, other.Value);
        }

        if (!Exists && !other.Exists)
        {
            return true;
        }

        return false;
    }

    public override int GetHashCode()
    {
        if (!Exists) return 0;

        return HashCode.Combine(Value, Exists, Unique);
    }
}