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
specification defines the Decimal32, Decimal64 and Decimal128 formats. In this library they are named DecimalSingle, 
DecimalDouble and DecimalQuad.

- **Standardized format**: Decimal floating-point is standardized in the IEEE-754 standard, which means that it is 
well-defined and portable across different computer systems and programming languages. This is mainly an advantage 
when compared to less portable, non-standardardized decimal formats.
