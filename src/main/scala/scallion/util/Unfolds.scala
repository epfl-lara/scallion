/* Copyright 2020 EPFL, Lausanne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scallion
package util

/** Contains functions to unfold values.
  *
  * @group other
  */
object Unfolds {

  /** Unapplies a `reduceLeft` operation.
    *
    * @param f Computes how to un-merge two elements.
    */
  def unreduceLeft[A](f: PartialFunction[A, (A, A)]): A => Seq[Seq[A]] = {
    def go(x: A): List[Vector[A]] = Vector(x) :: (f.lift(x) match {
      case None => List()
      case Some((l, r)) => go(l).map(_ :+ r)
    })

    go(_)
  }

  /** Unapplies a `reduceRight` operation.
    *
    * @param f Computes how to un-merge two elements.
    */
  def unreduceRight[A](f: PartialFunction[A, (A, A)]): A => Seq[Seq[A]] = {
    def go(x: A): List[List[A]] = List(x) :: (f.lift(x) match {
      case None => List()
      case Some((l, r)) => go(r).map(l :: _)
    })

    go(_)
  }

  /** Unapplies a `foldLeft` operation.
    *
    * @param f Computes how to un-merge two elements.
    */
  def unfoldLeft[A, B](f: PartialFunction[B, (B, A)]): B => Seq[B ~ Seq[A]] = {
    def go(x: B): List[B ~ Vector[A]] = (x ~ Vector()) :: (f.lift(x) match {
      case None => List()
      case Some((c, a)) => go(c).map {
        case z ~ as => z ~ (as :+ a)
      }
    })

    go(_)
  }

  /** Unapplies a `foldRight` operation.
    *
    * @param f Computes how to un-merge two elements.
    */
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