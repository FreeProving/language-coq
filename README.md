# language-coq

<!-- Badges -->
![CI Pipeline](https://github.com/FreeProving/language-coq/workflows/CI%20Pipeline/badge.svg)

<!-- Short description -->
This repository contains an AST and pretty-printer for Coq.
The code in this repository has originally been developed as part of the [hs-to-coq][] compiler but has been extracted for use in the [free-compiler][]

## Table of Contents

1. [Directory Structure](#directory-structure)
2. [Getting Started](#getting-started)
3. [Get Involved](#get-involved)
4. [License](#license)

## Directory Structure

This repository is structured as follows.

 - `./src/lib` contains the source code of the library.
    All modules start with the prefix `Language.Coq`.

    + The AST is located in `Language.Coq.Gallina`.
    + The pretty-printer can be found in `Language.Coq.Pretty`.

 - `tool` contains Bash scripts that are used during development and for testing.

## Getting Stated

### Required Software

The library is written in Haskell and uses Cabal to manage the dependencies.
The library has been tested with the following versions of the GHC and Cabal.

 - [GHC][software/ghc], version  8.6.5
 - [Cabal][software/cabal], version 3.4.0.0

### Installation

The `language-coq` package is not yet on Hackage.
In order to use this package in your own project, include the following stanza in your `cabal.project` file.

```cabal
source-repository-package
  type: git
  location: git://github.com/FreeProving/language-coq.git
  tag: v0.4.0.0
```

## Get Involved

Feature requests, enhancement proposals, bug reports, pull requests and all other contributions are welcome!  
Have a look at our [contributing guidelines][guidelines/CONTRIBUTING] for more information on how to contribute.

## License

The `language-coq` library is licensed under the MIT License.  
See the [LICENSE][language-coq/LICENSE] file for details.

[free-compiler]:
  https://github.com/FreeProving/free-compiler
  "Free Compiler on GitHub"

[guidelines/CONTRIBUTING]:
  https://github.com/FreeProving/guidelines/blob/main/CONTRIBUTING.md
  "Contributing Guidelines of the FreeProving project"

[hs-to-coq]:
  https://github.com/antalsz/hs-to-coq
  "hs-to-coq on GitHub"

[language-coq/LICENSE]:
  https://github.com/FreeProving/language-coq/blob/main/LICENSE
  "haskell-src-transformations — The MIT License"

[software/ghc]:
  https://www.haskell.org/ghc/
  "The Glasgow Haskell Compiler"
[software/cabal]:
  https://www.haskell.org/cabal/
  "Common Architecture for Building Applications and Libraries"
