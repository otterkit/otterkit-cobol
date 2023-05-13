# <img width="36" height="36" src="https://raw.githubusercontent.com/otterkit/otterkit/main/Assets/OtterkitIcon.png?sanitize=true&raw=true"> Otterkit COBOL Compiler

Otterkit is an effort to fully modernize the COBOL ecosystem by building a standard conforming implementation (of the [ISO/IEC 1989:2023](https://www.iso.org/standard/74527.html) standard) and to give developers access to modern Standard COBOL features, modern tooling and a better development experience.

The current situation in the COBOL ecosystem presents a unique opportunity for our project to create a unified and open source platform for COBOL development. We strongly believe that COBOL is capable of so much more than just being a legacy mainframe language, perpetually stuck with legacy standards.

Our biggest reason for advocating open source in the COBOL ecosystem is that while paying for a compiler may be feasible for larger companies, it currently creates a barrier for individual developers and students who wish to learn COBOL or stay up-to-date with its latest features. We want to change that, by making it accessible for everyone and hopefully helping modernize and unify ecosystem.

Otterkit and its runtime library are being built entirely in C# (with .NET 7). It compiles COBOL code into C# (with our runtime library) which is then compiled into an executable by the dotnet compiler.

Warning: The project is currently in pre-release, so not all of the standard has been implemented.

## About COBOL

COBOL was created in 1959 by the [CODASYL Committee](https://en.wikipedia.org/wiki/CODASYL) (With Rear Admiral Grace Hopper as a technical consultant to the committee), its design follows Grace Hopper's belief that programs should be written in a language that is close to English. It prioritizes readability, reliability, and long-term maintenance. The language has been implemented throughout the decades on many platforms with many dialects.

## Frequently Asked Questions

- What evidence do you have that there's demand for COBOL 2023?

There will never be a demand for it if developers can't get their hands on modern COBOL to try it out.

A good comparison for this is that there would be (most likely) no demand for Rust, if developers couldn't get their hands on Rust to try it out. Now that a lot of developers and companies did, there is a growing demand.

If developers are not aware of most modern COBOL features and no compiler supports it, how can there be any demand for these features?

It's almost like these features never existed, but we want to help improve this situation and bring more awareness to COBOL's more modern features.

## Installation

### Quick Install

Otterkit is available to install on the [Nuget package manager](https://www.nuget.org/packages/Otterkit/) ([.NET 7](https://dotnet.microsoft.com/en-us/download/dotnet/7.0) is required). To install, type these two lines into the command line:

```
dotnet new install Otterkit.Templates::1.0.45-alpha

dotnet tool install --global Otterkit --version 1.0.70
```

### Create and Build a COBOL Project

Create a new application (executable) project:

```
otterkit new app   
```

Build the project by specifying the entry point (main file):

```
otterkit build -e ${MAIN-FILE}.cob --free
```

Build and run the project by specifying the entry point (main file):

```
otterkit build --run -e ${MAIN-FILE}.cob --free
```

### Build from Source

First, run the git clone command, with the relevant arguments: 
```
git clone https://github.com/otterkit/otterkit.git --recurse-submodules --remote-submodules
```
The *recurse-submodules* and *remote-submodules* flags are needed to access the [libotterkit](https://github.com/otterkit/libotterkit) submodule inside.

Then, navigate into the `otterkit/src` folder (for the compiler, not libotterkit) and then type `dotnet run` into the command line to run and test if everything is working.

## Sponsors and Open Source Support

<h3 align="center">Open Source Support</h3>

<p align="center">
  <a target="_blank" href="https://www.jetbrains.com/community/opensource/">
    <img width="160" src="https://resources.jetbrains.com/storage/products/company/brand/logos/jb_beam.png" alt="JetBrains Logo (Main) logo.">
  </a>
</p>

## Standard Acknowledgement

Any organization interested in reproducing the COBOL standard and specifications in whole or in part,
using ideas from this document as the basis for an instruction manual or for any other purpose, is free
to do so. However, all such organizations are requested to reproduce the following acknowledgment
paragraphs in their entirety as part of the preface to any such publication (any organization using a
short passage from this document, such as in a book review, is requested to mention "COBOL" in
acknowledgment of the source, but need not quote the acknowledgment):

COBOL is an industry language and is not the property of any company or group of companies, or of any
organization or group of organizations.

No warranty, expressed or implied, is made by any contributor or by the CODASYL COBOL Committee
as to the accuracy and functioning of the programming system and language. Moreover, no
responsibility is assumed by any contributor, or by the committee, in connection therewith.

The authors and copyright holders of the copyrighted materials used herein:

- FLOW-MATIC® (trademark of Sperry Rand Corporation), Programming for the 'UNIVAC® I and
  II, Data Automation Systems copyrighted 1958,1959, by Sperry Rand Corporation;
- IBM Commercial Translator Form No F 28-8013, copyrighted 1959 by IBM;
- FACT, DSI 27A5260-2760, copyrighted 1960 by Minneapolis-Honeywell

Have specifically authorized the use of this material in whole or in part, in the COBOL specifications.
Such authorization extends to the reproduction and use of COBOL specifications in programming
manuals or similar publications.

## Contributing
If you have any questions, please feel free to reach out by [opening an issue](https://github.com/otterkit/otterkit/issues) or a [new discussion post](https://github.com/orgs/otterkit/discussions). We're happy to welcome new contributors!

Please keep in mind that the project is licensed under the Apache-2.0, and all contributions (unless otherwise specified) will be made available under the same terms (Apache-2.0, Submission of Contributions).

## Copyright 
Copyright © 2022-2023 Gabriel Gonçalves <KTSnowy@outlook.com>

Unless otherwise specified, all files in this repo (except the Otterkit logo) are licensed under the Apache-2.0 license.

See [LICENSE](https://github.com/otterkit/otterkit/blob/main/LICENSE) for the full license.

see [NOTICE](https://github.com/otterkit/otterkit/blob/main/NOTICE) for third-party software licenses.
