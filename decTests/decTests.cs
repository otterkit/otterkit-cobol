using Otterkit.Numerics;

var decquad = Decimal128.One + Decimal128.One;

Console.WriteLine(Decimal128.Pow(2, Decimal128.Pi));
Console.WriteLine(Math.Pow(2, Math.PI));

Span<byte> span = stackalloc byte[43];

var length = Decimal128.Pi.AsSpan(span);

Console.WriteLine(length);
