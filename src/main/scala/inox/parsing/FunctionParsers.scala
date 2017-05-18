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
trait FunctionParsers { self : Interpolator =>

  class FunctionParser extends ExpressionParser {

    import FunctionIR._

    val funName: Parser[Identifier] = positioned(acceptMatch("Identifier", {
      case lexical.Identifier(name) => IdentifierName(name)
    })) withFailureMessage ((p: Position) =>
      withPos("Identifier expected in Function Definition.", p)
      )

    val typeParam: Parser[TypeParam] = positioned(acceptMatch("TypeParam", {
        case lexical.Identifier(t) => IdentifierName(t)
      })) withFailureMessage ((p: Position) => withPos("Type parameters expected.", p))

    val templateParams: Parser[Seq[TypeParam]] = p('[') ~>
      repsep(typeParam, p(',')) <~
      p(']') withFailureMessage ((p: Position) =>
      withPos("Function Type Parameters expected", p))

    val funArg: Parser[Argument] = for {
      id <- commit(funName)
      _ <- p(':')
      ty <- commit(typeParam)
    } yield Argument(id, ty)

    val funArgs: Parser[Seq[Argument]] = p('(') ~> repsep(funArg, p(',')) <~ p(')')

    val fun : Parser[Function] = for {
      _ <- kw("def")
      name <- commit(funName withFailureMessage {
        (p: Position) => withPos("Missing identifier for functions", p)
      })
      templateParams <- commit(opt(templateParams) withFailureMessage((p: Position) =>
        withPos("Missing type parameters", p)))
      arguments <- commit(opt(funArgs) withFailureMessage((p: Position) =>
        withPos("Missing arguments", p)))
      _ <- p(':')
      returnType <- commit(typeParam)
      _ <- kw("=")
      _ <- p('{')
      body <- rep(expression)
      _ <- p('}')
    } yield Function(name, templateParams.getOrElse(Seq.empty), arguments.getOrElse(Seq.empty), returnType, body)


  }

}
