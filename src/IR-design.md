# Otterkit Intermediate Representation Design

The Otterkit Intermediate Representation (IR) is a language agnostic representation of a COBOL program, meant to be used as an internal input for the code generator. The IR is designed to allow for easier code generation to multiple target languages, and to allow for easier optimization and lowering language features to simpler constructs.

## Design Goals

The design goals of the IR are:

- Easy to generate directly from the parser.
- Easy to use as input for the code generator.
- Moderately easy to read and understand.
- Minimize the amount of keywords the code generator needs to handle.
- Allow for easier optimization and lowering language features to simpler constructs.

## Current Design

This section describes the current design of the intermediate representation. The design is subject to change as we continue to develop and improve the IR.

### Defining an IR unit

A unit is defined by the `unit` keyword followed by the type of unit and the name of the unit. The type of unit can be `program`, `function`, `class`, or `interface`. The name of the unit is a global unique identifier prefixed by `@`.

```
unit program @hello-world
```

Program and function units must have a single `procedure` block. Class and interface units may have multiple `procedure` blocks, each defining a method. For the latter, the local variables for each method immediately precede the `procedure` block defining the method.

### Defining an IR variable

An IR variable is defined using the `automatic` `static`, `initial`, `parameter` or `return` keywords followed by the level number, the index of the variable (order in which it was defined) in the data division prefixed by `%v` (replaces the name, e.g. `%v0`), the class[type] of the variable (e.g. `fixed[999]`) and any other attributes (e.g. `occurs[10]`).

```
static 01 $0 fixed[999]
```

`automatic` `static`, `initial` refer to the life-time of variable. `automatic` variables are allocated on the stack on every unit call, `static` variables are allocated on the heap, staying in a last-used state between unit calls, and `initial` variables are allocated on the heap, being set to an initial state on every unit call. `parameter` and `return` refer to the parameters and return value of a procedure.

`static` variables are not shared between object instances, each object instance has its own copy of the variable.

`automatic` variables are only valid inside the procedure block they are defined in, and for objects, they are only valid inside the procedure block of the method they are defined in.

`initial` variables only exist in program units defined with the `initial` attribute.

`parameter` and `return` variables share the same life-time as the variables passed in by the caller.

Note: the `return` variable is also passed in by the caller, and so its life-time is not the same as a local automatic variable's life-time. This is unlike other languages, where the return variable is allocated by the callee and passed back to the caller.

See *8.6.4 Automatic, initial, and static internal items* in the COBOL 2023 standard for more information.

### Defining an IR procedure

An IR procedure is defined using the `procedure` keyword followed by the parameter list and the return value. The parameter list starts with the `params` keyword followed by the list of parameters. The return value starts with the `ret` keyword followed by the return value. When there are no parameters or return value, the list only contains a percent sign (`%`). The parameter list and return value are separated by the `|` character, each parameter is separated by a space.

```
procedure params|%| ret|%| =>
```

Paragraphs and sections are defined using the `paragraph` and `section` keywords followed the index of the label (order in which it was defined) in the procedure division prefixed by `%l` (replaces the name, e.g. `%l0`).

```
[section %l0]

[paragraph %l1]
```

### Defining an IR statement

An IR statement is defined using the name of the statement followed by lists of "argument" values (e.g. `|"Hello"| |$0|`). The values of each list are separated inside a `| |` (e.g. `|arg arg ...|`), each argument is separated by a space. The arguments can be a literal, a variable, or a label. A literal can be any valid COBOL literal, a variable is a variable defined in the data division, and a label is a paragraph or section name.

Statements are surrounded by square brackets (`[` and `]`). The name of the statement is followed by the lists of argument values for that particular statement.

```
[display |"Hello"| |$0|]
```

Statements can be nested inside other statements. The nested statements are also surrounded by square brackets. Arithmetic and conditional expressions are considered argument values, also separated inside a `| |`.

```
[if |$0 > 0|
    [display |"Hello"| |$0|]
else
    [display |"Goodbye"| |$0|]]
```

### Lowering language features to simpler constructs

Certain language features can be lowered to simpler constructs without losing any features. For example, certain `perform` statements can be lowered to simpler forms. The following `perform` statement, which is equivalent to a for loop:

```cobol
PERFORM VARYING X FROM 1 BY 1 UNTIL X > Y
. . .
END-PERFORM
```

Can be lowered to the following form, which is equivalent to a while loop with manual incrementing:

```cobol
COMPUTE X = 1
PERFORM UNTIL X > Y

    COMPUTE X = X + 1
END-PERFORM
```

Certain statements can be completely removed from the IR. For example, the arithmetic `add` statement can be lowered to a simpler (for the runtime) `compute` statement. The following `add` statement:

```cobol
ADD 1 TO X
```

Is equivalent to the following `compute` statement:

```cobol
COMPUTE X = X + 1
```

Lowering all arithmetic statements to specific combinations of `compute` could simplify both the code generator and runtime library, as they would only need to handle a single arithmetic statement as opposed to 5. No features would be lost, as all arithmetic statements can be lowered cleanly to one or two `compute` statements.

More examples of lowering will be added here as a form of documentation as we continue to develop the IR. The goal is to lower as many language features as possible to simpler constructs, without losing any features.
