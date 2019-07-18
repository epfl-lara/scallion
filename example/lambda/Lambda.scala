package example.lambda

import scallion.parsing._

sealed abstract class Token
case object LambdaToken extends Token
case object DotToken extends Token
case class NameToken(name: String) extends Token
case class ParensToken(isOpen: Boolean) extends Token

sealed abstract class Kind
case object LambdaKind extends Kind
case object DotKind extends Kind
case object NameKind extends Kind
case class ParensKind(isOpen: Boolean) extends Kind

sealed abstract class Expr
case class Var(name: String) extends Expr
case class App(left: Expr, right: Expr) extends Expr
case class Abs(name: String, body: Expr) extends Expr

object Parser extends Parsers[Token, Kind] {
  override def getKind(token: Token): Kind = token match {
    case LambdaToken => LambdaKind
    case DotToken => DotKind
    case NameToken(_) => NameKind
    case ParensToken(isOpen) => ParensKind(isOpen)
  }

  val open = accept(ParensKind(true)) {
    case _ => ()
  } withInverse {
    case () => ParensToken(true)
  }

  val close = accept(ParensKind(false)) {
    case _ => ()
  } withInverse {
    case () => ParensToken(false)
  }

  val lambda = accept(LambdaKind) {
    case _ => ()
  } withInverse {
    case () => LambdaToken
  }

  val dot = accept(DotKind) {
    case _ => ()
  } withInverse {
    case () => DotToken
  }

  val name = accept(NameKind) {
    case NameToken(n) => n
  } withInverse {
    case n => NameToken(n)
  }

  val variable = transform(name) {
    case n => Var(n)
  } withInverse {
    case Var(n) => n
  }

  lazy val basic: Parser[Expr] = variable | open ~>~ expr ~<~ close

  lazy val args: Parser[Seq[Expr]] = recursive {
    basic +: (args | epsilon(Seq())) | lambdaExpr +: epsilon(Seq())
  }

  lazy val appExpr: Parser[Expr] = transform(args) {
    case xs => xs.reduceLeft(App(_, _))
  } withInverses {
    case e: Expr => {
      def go(e: Expr): Seq[Seq[Expr]] = Seq(e) +: (e match {
        case App(l, r) => go(l).map {
          case ls => ls :+ r
        }
        case _ => Seq()
      })

      go(e)
    }
  }

  lazy val expr: Parser[Expr] = recursive {
    appExpr
  }

  lazy val lambdaExpr: Parser[Expr] = (lambda ~>~ many1(name) ~<~ dot ~ expr).map({
    case ns ~ b => ns.foldRight(b)(Abs(_, _))
  }, {
    case e: Expr => {
      def go(e: Expr): Seq[Seq[String] ~ Expr] = (Seq() ~ e) +: (e match {
        case Abs(n, b) => go(b).map {
          case ns ~ z => (n +: ns) ~ z
        }
        case _ => Seq()
      })

      go(e)
    }
    case _ => Seq()
  })

  def pretty(tokens: Seq[Token]): String = {

    val space: ((Token, Token)) => String = {
      case (NameToken(_), NameToken(_)) => " "
      case _ => ""
    }

    val spaces = "" +: tokens.zip(tokens.tail).map(space)

    val strings = tokens.map {
      case LambdaToken => "\\"
      case NameToken(n) => n
      case DotToken => "."
      case ParensToken(isOpen) => if (isOpen) "(" else ")"
    }

    spaces.zip(strings).map(x => x._1 + x._2).mkString("")
  }
}