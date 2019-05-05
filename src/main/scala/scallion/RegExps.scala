package scallion

/** Contains definitions relating to regular expressions. */
trait RegExps {

  type Character

  import RegExp._

  /** Regular expressions over characters. */
  sealed abstract class RegExp {

    /** Checks whether this regular expression accepts the empty word. */
    val acceptsEmpty: Boolean

    /** Returns the rest of a regular expression after consuming a character. */
    def derive(char: Character): RegExp = this match {
      case EmptyStr => EmptySet
      case EmptySet => EmptySet
      case Elem(predicate) =>
        if (predicate(char)) EmptyStr
        else EmptySet
      case Union(left, right) => left.derive(char) | right.derive(char)
      case Concat(first, second) =>
        if (first.acceptsEmpty) first.derive(char) ~ second | second.derive(char)
        else first.derive(char) ~ second
      case Star(regExp) => regExp.derive(char) ~ this
    }

    /** Union of `this` and `that` regular expression.
     *
     * Performs surface-level simplifications.
     *
     * @return A regular expression that accepts words if either `this` or `that` accepts it.
     */
    def |(that: RegExp): RegExp = (this, that) match {
      case (EmptySet, _) => that
      case (_, EmptySet) => this
      case _ => Union(this, that)
    }

    /** Concatenation of `this` and `that` regular expression.
     *
     * Performs surface-level simplifications.
     *
     * @return A regular expression that accepts words
     *.        if `this` accepts a prefix and `that` accepts the suffix.
     */
    def ~(that: RegExp): RegExp = (this, that) match {
      case (EmptySet, _) => EmptySet
      case (_, EmptySet) => EmptySet
      case (EmptyStr, _) => that
      case (_, EmptyStr) => this
      case _ => Concat(this, that)
    }
  }

  /** Contains primitive constructors for regular expressions. */
  object RegExp {

    /** Accepts only the empty word. */
    case object EmptyStr extends RegExp {
      override val acceptsEmpty: Boolean = true
    }

    /** Never accepts. */
    case object EmptySet extends RegExp {
      override val acceptsEmpty: Boolean = false
    }

    /** Accepts single character that satisfy a predicate. */
    case class Elem(predicate: Character => Boolean) extends RegExp {
      override val acceptsEmpty: Boolean = false
    }

    /** Union of two regular expressions. */
    case class Union(left: RegExp, right: RegExp) extends RegExp {
      override val acceptsEmpty: Boolean = left.acceptsEmpty || right.acceptsEmpty
    }

    /** Concatenation of two regular expressions. */
    case class Concat(first: RegExp, second: RegExp) extends RegExp {
      override val acceptsEmpty: Boolean = first.acceptsEmpty && second.acceptsEmpty
    }

    /** Kleene star of a regular expressions. */
    case class Star(regExp: RegExp) extends RegExp {
      override val acceptsEmpty: Boolean = true
    }
  }

  //---- Combinators ----//

  /** Regular expression that accepts any of the characters in `chars`. */
  def elem(chars: Seq[Character]): RegExp = {
    val empty: RegExp = EmptySet

    chars.foldRight(empty) {
      case (char, rest) => elem(char) | rest
    }
  }

  /** Regular expression that accepts single characters based on a `predicate`. */
  def elem(predicate: Character => Boolean): RegExp = Elem(predicate)

  /** Regular expression that accepts only the single character `char`. */
  def elem(char: Character): RegExp = Elem(_ == char)

  /** Regular expression that accepts only the sequence of characters `chars`. */
  def word(chars: Seq[Character]): RegExp = {
    val empty: RegExp = EmptyStr
    chars.foldRight(empty) {
      case (char, rest) => elem(char) ~ rest
    }
  }

  /** Zero or more repetitions of the regular expression `regExp`. */
  def many(regExp: RegExp): RegExp = Star(regExp)

  /** One or more repetitions of the regular expression `regExp`. */
  def many1(regExp: RegExp): RegExp = regExp ~ many(regExp)

  /** Zero or one instances of the regular expression `regExp`. */
  def opt(regExp: RegExp): RegExp = regExp | EmptyStr
}