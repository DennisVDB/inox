package inox
package parsing

import scala.util.parsing.input.Positional

/**
  * Created by junze on 5/15/17.
  */
// probably needs to extend FunctionElaborators
trait FunctionIRs { self: Interpolator =>
  object FunctionIR extends IR {
    type Operator = Nothing
    type Quantifier = Nothing
    type Field = Nothing
    type Value = Nothing

    type Type = TypeIR.Expression
    type Statement = ExprIR.Expression

    case class TypeParam(name: String) {
      def getName: String = name
    }

    sealed abstract class Identifier extends Positional {
      def getName: String
      def getShortName: String

      override def toString = pos + "@" + getName
    }
    case class IdentifierName(name: String) extends Identifier {
      override def getName = name
      override def getShortName = name
    }

    case class Arg(id: Identifier, tpe: Type)

    case class Function(id: Identifier,
                        typeParams: Seq[TypeParam],
                        args: Seq[Arg],
                        retType: Type,
                        body: Statement)
  }
}
