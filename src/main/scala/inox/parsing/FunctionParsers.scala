package inox
package parsing

import scala.util.parsing.input.Position

/**
  * Syntax
  *
  * def foo[T1, T2](a : Int, b : T1) : T2 = {
  *   ...
  * }
  */
/**
  * Created by junze on 5/15/17.
  */
trait FunctionParsers { self: Interpolator =>

  class FunctionParser extends ExpressionParser {

    import FunctionIR._

    val funName: Parser[Identifier] = positioned(acceptMatch("Identifier", {
      case lexical.Identifier(name) => IdentifierName(name)
    })) withFailureMessage ((p: Position) =>
      withPos("Identifier expected in Function Definition.", p))

    lazy val typeParam: Parser[TypeParam] = for {
      v <- opt(variance)
      n <- acceptMatch("TypeParam", {
        case lexical.Identifier(t) => t
      })
    } yield TypeParam(n, v.getOrElse(Invariant))

    lazy val variance: Parser[Variance] =
      lexical.Operator("+") ^^^ Covariant | lexical.Operator("-") ^^^ Contravariant

//    val typeParam: Parser[TypeParam] = positioned(acceptMatch("TypeParam", {
//      case lexical.Identifier(t) => IdentifierName(t)
//    })) withFailureMessage ((p: Position) =>
//      withPos("Type parameters expected.", p))

    val typeParams: Parser[Seq[TypeParam]] = p('[') ~>
      repsep(typeParam, p(',')) <~
      p(']') withFailureMessage ((p: Position) =>
      withPos("Function Type Parameters expected", p))

    val funArg: Parser[Arg] = for {
      id <- commit(funName)
      _ <- p(':')
      tpe <- commit(typeExpression)
    } yield Arg(id, tpe)

    val funArgs: Parser[Seq[Arg]] = p('(') ~> repsep(funArg, p(',')) <~ p(')')

    val fun: Parser[Function] = for {
      _ <- kw("def")
      name <- commit(funName withFailureMessage { (p: Position) =>
        withPos("Missing identifier for functions", p)
      })
      templateParams <- commit(
        opt(typeParams) withFailureMessage ((p: Position) =>
          withPos("Missing type parameters", p)))
      arguments <- commit(opt(funArgs) withFailureMessage ((p: Position) =>
        withPos("Missing arguments", p)))
      _ <- p(':')
      returnType <- commit(typeExpression)
      _ <- kw("=")
      _ <- p('{')
      body <- rep(expression)
      _ <- p('}')
    } yield
      Function(name,
               templateParams.getOrElse(Seq.empty),
               arguments.getOrElse(Seq.empty),
               returnType,
               body)

  }

}
