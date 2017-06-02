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

    val dataTypes: Parser[List[DataType]] = rep1(dataType)

    lazy val dataType: Parser[DataType] = for {
      _ <- kw("type")
      id <- identifier
      tParams <- opt(typeParams)
      _ <- kw("=")
      cs <- constructors
    } yield DataType(id, tParams.getOrElse(Seq.empty), cs)

    lazy val identifier: Parser[Identifier] = acceptMatch("dataTypeId", {
      case lexical.Identifier(id) => id
    })

    lazy val typeParams: Parser[List[TypeParam]] = p('[') ~> repsep(
      typeParam,
      p(',')) <~ p(']')

    lazy val typeParam: Parser[TypeParam] = for {
      v <- opt(variance)
      n <- acceptMatch("typeParam", {
        case lexical.Identifier(t) => t
      })
    } yield TypeParam(n, v.getOrElse(Invariant))

    lazy val variance: Parser[Variance] =
      lexical.Operator("+") ^^^ Covariant | lexical.Operator("-") ^^^ Contravariant

    lazy val constructors: Parser[List[ValueConstructor]] =
      rep1sep(constructor, elem(lexical.Operator("|")))

    lazy val constructor: Parser[ValueConstructor] = for {
      id <- identifier
      args <- opt(arguments)
    } yield ValueConstructor(id, args.getOrElse(Seq.empty))

    val arguments: Parser[List[Arg]] =
      p('(') ~> repsep(argument, p(',')) <~ p(')')

    lazy val argument: Parser[Arg] = for {
      id <- identifier
      _ <- p(':')
      tpe <- typeExpression
    } yield Arg(id, tpe)
  }

}
