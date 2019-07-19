package scallion.parsing

package object unfolds {

  def unreduceLeft[A](f: PartialFunction[A, (A, A)]): A => Seq[Seq[A]] = {
    def go(x: A): List[Vector[A]] = Vector(x) :: (f.lift(x) match {
      case None => List()
      case Some((l, r)) => go(l).map(_ :+ r)
    })

    go(_)
  }

  def unreduceRight[A](f: PartialFunction[A, (A, A)]): A => Seq[Seq[A]] = {
    def go(x: A): List[List[A]] = List(x) :: (f.lift(x) match {
      case None => List()
      case Some((l, r)) => go(r).map(l :: _)
    })

    go(_)
  }

  def unfoldLeft[A, B](f: PartialFunction[B, (B, A)]): B => Seq[B ~ Seq[A]] = {
    def go(x: B): List[B ~ Vector[A]] = (x ~ Vector()) :: (f.lift(x) match {
      case None => List()
      case Some((c, a)) => go(c).map {
        case z ~ as => z ~ (as :+ a)
      }
    })

    go(_)
  }

  def unfoldRight[A, B](f: PartialFunction[B, (A, B)]): B => Seq[Seq[A] ~ B] = {
    def go(x: B): List[List[A] ~ B] = (List() ~ x) :: (f.lift(x) match {
      case None => List()
      case Some((a, c)) => go(c).map {
        case as ~ z => (a :: as) ~ z
      }
    })

    go(_)
  }
}