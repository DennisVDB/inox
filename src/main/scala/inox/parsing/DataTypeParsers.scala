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
    import DataTypeIR._

    val dataTypes: Parser[List[DataTypeSort]] = rep1(dataType)

    lazy val dataType: Parser[DataTypeSort] = for {
      _ <- kw("type")
      id <- identifier
      tParams <- opt(typeParams)
      _ <- kw("=")
      cs <- constructors
    } yield DataTypeSort(id, tParams.getOrElse(Seq.empty), cs)

    lazy val identifier: Parser[Identifier] = acceptMatch("dataTypeId", {
      case lexical.Identifier(id) => Identifier(id)
    })

    lazy val typeParams: Parser[List[TypeParam]] = p('[') ~> repsep(
      typeParam,
      kw(",")) <~ p(']')

    lazy val typeParam: Parser[TypeParam] = acceptMatch("typeParam", {
      case lexical.Identifier(t) => t
    })

    lazy val constructors: Parser[List[DataTypeConstructor]] =
      rep1sep(constructor, kw("or"))

    lazy val constructor: Parser[DataTypeConstructor] = for {
      id <- identifier
      tParams <- opt(typeParams)
      args <- opt(arguments)
    } yield
      DataTypeConstructor(id,
                          tParams.getOrElse(Seq.empty),
                          args.getOrElse(Seq.empty))

    lazy val arguments: Parser[List[Argument]] = p('(') ~> repsep(argument,
                                                                  p(',')) <~ p(
      ')')

    lazy val argument: Parser[Argument] = for {
      id <- identifier
      _ <- p(':')
      tpe <- typeExpression
    } yield Argument(id, tpe)
  }

}
