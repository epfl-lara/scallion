package example.python

import scallion._

sealed trait Token
case class Space(value: String) extends Token
case object NewLineToken extends Token
case object IdentToken extends Token
case object DedentToken extends Token
case class DelimiterToken(value: String) extends Token
case class IdentifierToken(value: String) extends Token
case class OperatorToken(value: String) extends Token
case class KeywordToken(value: String) extends Token
case class CommentToken(value: String) extends Token
case class StringLiteralToken(value: String) extends Token
case class NumberLiteralToken(value: String) extends Token