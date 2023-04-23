using System.Diagnostics.CodeAnalysis;

namespace Otterkit.Types;

public readonly struct Unique<TValue>
    where TValue : notnull
{
    internal readonly TValue Value;
    public readonly bool IsUnique;
    public readonly bool Exists;

    public static Unique<TValue> Null
    {
        get => new Unique<TValue>();
    }

    public Unique(TValue value, bool isUnique)
    {
        Value = value;
        Exists = true;

        IsUnique = isUnique;
    }

    public bool TryGetValue([NotNullWhen(true)] out TValue? value)
    {
        if (Exists)
        {
            value = Value;
            return true;
        }

        value = default;
        return false;
    }

    public static implicit operator Unique<TValue>(TValue? value)
    {
        if (value is null) return Null;

        return new Unique<TValue>(value, true);
    }

    public static implicit operator Unique<TValue>((TValue value, bool isUnique) tuple)
    {
        if (tuple.value is null) return Null;

        return new Unique<TValue>(tuple.value, tuple.isUnique);
    }

    public static explicit operator TValue(Unique<TValue> Unique)
    {
        if (Unique.Exists) return Unique.Value;

        throw new NullReferenceException("Instance of Unique did not have a value");
    }
}
