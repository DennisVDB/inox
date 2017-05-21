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
    type TypeParam = Identifier
    type Statement = ExprIR.Expression

    sealed abstract class Identifier extends Positional {
      def getName: String
      def getShortName: String

      override def toString = pos + "@" + getName
    }
    case class IdentifierName(name: String) extends Identifier {
      override def getName = name
      override def getShortName = name
    }

    case class Argument(id: Identifier, ty: TypeParam)
    case class Function(name: Identifier,
                        typeParams: Seq[TypeParam],
                        arguments: Seq[Argument],
                        returnType: TypeParam,
                        body: Seq[Statement])
  }
}
