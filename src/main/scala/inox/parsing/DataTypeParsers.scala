package inox
package parsing

import scala.util.parsing.combinator.RegexParsers

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

    lazy val identifier: Parser[Identifier] = acceptMatch("Name", {
      case lexical.Identifier(name) => Identifier(name)
    })

    lazy val tpeArg: Parser[Type] = acceptMatch("Types", {
      case lexical.Identifier(tpe) => tpe
    })

    lazy val tpeArgs
      : Parser[Seq[Type]] = p('[') ~> repsep(tpeArg, kw(",")) <~ p(']')

    val argument: Parser[(Identifier, Type)] = for {
      id <- identifier
      _ <- kw(":")
      tpe <- tpeArg
    } yield (id, tpe)

    val arguments: Parser[Seq[(Identifier, Type)]] = p('(') ~> repsep(
      argument,
      kw(",")) <~ p(')')

    val constructor: Parser[DataConstructor] = for {
      id <- identifier
      tps <- tpeArgs
      args <- arguments
    } yield DataConstructor(id, tps, args)

    val constructors: Parser[Seq[DataConstructor]] =
      rep1sep(constructor, kw("|"))

    val dataType: Parser[TypeConstructor] = {
      for {
        _ <- kw("type")
        id <- commit(identifier)
        tps <- commit(tpeArgs)
        _ <- kw("=")
        cons <- commit(constructors)
      } yield TypeConstructor(id, tps, cons)
    }
  }

}
