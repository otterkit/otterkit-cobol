# Otterkit COBOL Compiler

Otterkit is a Free and Open Source compiler for the [COBOL Language](https://en.wikipedia.org/wiki/COBOL#History_and_specification) on the .NET platform. Warning: The project is currently in pre-release, so not all of the standard has been implemented.

## About

First created in 1959, COBOL was created so that managers could communicate with programmer about code. It prioritizes readability, reliability, and long-term maintinence. The language has been implemented throughout the decades on many platforms with many dialects, but the Otterkit compiler is a Free and Open Source implementation of the ISO 2023 Standard on the .NET platform. 

## Installation

### Quick Install

Otterkit is avalible to install on the [Nuget package manager](https://www.nuget.org/packages/Otterkit/). To install, [.NET 7](https://dotnet.microsoft.com/en-us/download/dotnet/7.0) is required. To install, type in to the command line:
```
dotnet tool install --global Otterkit --version 1.0.0-alpha
```

### Build from Source

First, clone the git repo from [https://github.com/otterkit/otterkit.git](https://github.com/otterkit/otterkit.git) to get the source code. To access the libotterkit submodule inside, use the `--recurse-submodules --remote-submodules` flag on the clone command. To run, navigate into the `src` folder(for the compiler, not libotterkit) and then type `dotnet run` into the command line.