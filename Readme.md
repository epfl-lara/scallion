<p align="center">
<img src="images/scallion.png" width="300px" alt="SCALL1ON" />
</p>

# Overview

Scallion is a library for writing parsers and pretty printers for LL(1) languages in Scala.

# Documentation

A comprehensive Scaladoc API is [available online](https://epfl-lara.github.io/scallion/).

# Examples

* [JSON](example/json/JSON.scala): This example showcases how to build a basic JSON lexer & parser using Scallion.
* [Lambda Calculus](example/lambda/Lambda.scala): This example shows how to get a pretty printer *almost for free* alongside your parser. 
* [Calculator](example/calculator/Calculator.scala): This example shows how to use the `operators` combinator to easily handle operators with various associativities and priorities.
* [Roman Numerals](example/roman/Roman.scala): This example presents a simple parser and pretty printer for roman numerals.