# Changelog

## 0.4.0.0 / 2020-09-30

 - **Added support for `Context` sentences**
   + `Context` sentences have been added to the Coq AST that allow implicit arguments to be defined in `Section`s.

## 0.3.0.0 / 2020-09-25

 - **Support hints**
   + `Hint` sentences have been added to the Coq AST that support a wide range of hints.
 - **Support options and flags**
   + Option sentences (i.e., `Set` and `Unset`) have been added to the Coq AST that allow to set and unset arbitrary options and flags.

## 0.2.0.0 / 2020-08-22

 - **Extended notation definitions**
   + Notation definitions support a list of identifiers instead of a single identifier.
   + Notation definitions support a list of syntax modifiers.

## 0.1.0.0 / 2020-04-19

 - **Initial release** as `language-coq`

## 0.0.0.0 / 2019-05-16

 - **Initial version** forked from [hs-to-coq][]

[hs-to-coq]:
 https://github.com/antalsz/hs-to-coq
 "hs-to-coq on GitHub"
