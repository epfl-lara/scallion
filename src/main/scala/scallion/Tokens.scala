package scallion

trait Tokens {

  /** Type of tokens. */
  type Token

  /** The error token. */
  val ErrorToken: Token

  /** The end token. */
  val EndToken: Token
}