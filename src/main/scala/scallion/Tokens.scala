package scallion

trait Tokens[Token] {

  /** The error token. */
  val ErrorToken: Token

  /** The end token. */
  val EndToken: Token
}