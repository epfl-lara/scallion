// package scallion.syntactic

// trait Focuses { self: Syntaxes with Enumeration =>

//   import Syntax._

//   private[scallion] object Focus {
//     type DisId = Int

//     def find[A, B](hay: Syntax[A], needle: Syntax[B]): Seq[Context] = {
//       var recs: Map[RecId, InRec] = Map.empty
//       var res: Seq[Context] = Seq.empty

//       var disId: DisId = 0

//       def go[C](hay: Syntax[C], parent: Context): Unit = {
//         if (hay eq needle) {
//           res :+= parent
//         }
//         else {
//           println(hay)

//           hay match {
//             case Transform(_, _, inner) => go(inner, parent)
//             case Marked(_, inner) => go(inner, parent)
//             case Disjunction(left, right) => {
//               val newParent = parent match {
//                 case InDis(_, _) => parent
//                 case _ => {
//                   val id = disId
//                   disId += 1
//                   InDis(id, parent)
//                 }
//               }
//               go(left, newParent)
//               go(right, newParent)
//             }
//             case Sequence(left, right) => {
//               go(left, InLeft(right, parent))
//               go(right, InRight(left, parent))
//             }
//             case Recursive(id, inner) => {
//               recs.get(id) match {
//                 case Some(inRec) => inRec.parents :+= parent
//                 case None => {
//                   val newParent = new InRec
//                   newParent.parents :+= parent
//                   recs += (id -> newParent)
//                   go(inner, newParent)
//                 }
//               }
//             }
//             case _ => ()
//           }
//         }
//       }

//       go(hay, Top)

//       res
//     }

//     sealed trait Context
//     case object Top extends Context
//     case class InDis(id: DisId, parent: Context) extends Context
//     case class InLeft(right: Syntax[_], parent: Context) extends Context
//     case class InRight(right: Syntax[_], parent: Context) extends Context
//     class InRec extends Context {
//       var parents: Seq[Context] = Seq()

//       override def toString: String = "InRec(...)"
//     }
//   }
// }