using Otterkit.Numerics;

var OnePointFive = new Decimal128("1.5");

Console.WriteLine(Decimal128.Round(OnePointFive, (RoundingMode)9));
