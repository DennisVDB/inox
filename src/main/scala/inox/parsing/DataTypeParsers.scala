package inox
package parsing

/**
  * Created by junze on 5/1/17.
  */
// data Boolean = False | True -> p1
// data Tree[A] = Leaf(a: A) | Node(l: Tree[A], r: Tree[A]) -> p2
// data Foo = Bar(i: Int) | Baz(s: String) -> p3
// p = p1 | p2 | p3

/*
  sealed trait Tree[A]
  case class Leaf(a: A) extends Tree[A]
  case class Node...

  sealed trait Boolean
  object False extends Boolean
  object True extends Boolean
 */

trait DataTypeParsers { self: Interpolator =>

  class DataTypeParser extends TypeParser {

    import inox.parsing.DataTypeIR._

    lazy val tpe: Parser[Identifier] = ???

    lazy val tpeArgs: Parser[Seq[Type]] = ???

    val constructor: Parser[DataConstructor] = ???

    val constructors: Parser[Seq[DataConstructor]] = rep1(kw("|") ~> constructor)

    val dataType: Parser[TypeConstructor] = {
      for {
        _ <- kw("type")
        id <- commit(tpe)
        tps <- commit(tpeArgs)
        _ <- kw("=")
        cons <- commit(constructors)
      } yield TypeConstructor(id, tps, cons)
    }
  }

}
