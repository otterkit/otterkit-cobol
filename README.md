# Otterkit Standard Decimal Math Library

This library provides a (.NET 7) software implementation for the IEEE-754 decimal floating-point specification. 
Decimal floating-point can represent decimal numbers exactly, without accumulating rounding errors, in contrast to 
binary floating-point which can only approximate decimal numbers (causing rounding errors in the process).

A very famous example of this is the following expression `0.1 + 0.2` not being equal to `0.3` in most languages. 
That's because binary floating-point is not able to represent the number `0.3` exactly, which leaves a very small 
(but accumulating) rounding error, and so `0.1 + 0.2` is actually equal to `0.30000000000000004`.

## Advantages of decimal floating-point

There are numerous advantages of using decimal floating-point, especially in financial, commercial, and scientific 
applications where exact decimal calculations are needed (or required by law). Some of the main advantages are:

- **Exact representation of decimal numbers**: Decimal floating-point can represent decimal numbers exactly, without 
any rounding errors or approximation. This is particularly important in financial and accounting calculations, where 
rounding errors can accumulate and cause significant discrepancies (we need `$0.30` exactly, not `$0.30000000000000004`).

- **Easy human comprehension**: Decimal numbers are familiar to most people and are easier to comprehend than a 
binary approximation, especially when dealing with fractional values. We shoudln't have to constantly deal with 
rounding errors if we can store and run calculations as an exact decimal number.

- **Flexible sizes**: Decimal floating-point supports the same three format sizes as binary floating-point. The IEEE-754 
specification defines the Decimal32, Decimal64 and Decimal128 formats.

- **Standardized format**: Decimal floating-point is standardized in the IEEE-754 standard, which means that it is 
well-defined and portable across different computer systems and programming languages. This is mainly an advantage 
when compared to less portable, non-standardardized decimal formats.

## Installation and usage

### NuGet Install

Otterkit.Numerics is available to install on the [Nuget package manager](https://www.nuget.org/packages/Otterkit/) ([.NET 7](https://dotnet.microsoft.com/en-us/download/dotnet/7.0) is required). To install the latest version, type the following into the command line:
```
dotnet add package Otterkit.Numerics
```

### Example usage

Adding `0.1` to `0.2` to get an exact representation of `0.3`:
```csharp
// Create a new Decimal128 from a UTF-8 string literal
var pointOne = new Decimal128("0.1"u8);
var pointTwo = new Decimal128("0.2"u8);

// Arithmetic operators are available for the decimal types
var exactPointThree = pointOne + pointTwo;

// Will print exactly 0.3, ToString does not need to remove rounding errors
Console.WriteLine(exactPointThree);
```

Some common constants are also available:
```csharp
var E = Decimal128.E;
var Pi = Decimal128.Pi;
var Tau = Decimal128.Tau;

// E will print exactly: 2.718281828459045235360287471352662
Console.WriteLine(E);

// Pi will print exactly: 3.141592653589793238462643383279503
Console.WriteLine(Pi);

// Tau will print exactly: 6.283185307179586476925286766559006
Console.WriteLine(Tau);
```

A method is provided for printing an engineering string if needed:
```csharp
var scientificNotation = new Decimal128("4.5E10"u8);

// Will print: 4.5E+10
Console.WriteLine(scientificNotation);

// Will print: 45E+9
Console.WriteLine(scientificNotation.ToEngineeringString());
```
