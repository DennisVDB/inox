package inox
package parsing

import scala.util.parsing.input.Positional

/** Contains abstract Intermediate Representation (IR) language for data-types. */
trait DataTypeIRs extends DataTypeElaborators { self: Interpolator =>
  object DataTypeIR extends IR with DataTypeElaborator {

    case class Identifier(name: String)

    type Type = TypeIR.Expression
    type TypeParam = String

    case class Argument(id: Identifier, tpe: Type)

    case class DataTypeConstructor(id: Identifier,
                                   tps: Seq[TypeParam],
                                   args: Seq[Argument])

    abstract class DataType(pre: String) extends Positional with Product {
      override def productPrefix: String = pos + "@" + pre
    }

    case class DataTypeSort(id: Identifier,
                            tps: Seq[TypeParam],
                            constructors: Seq[DataTypeConstructor])
        extends DataType("TypeConstructor")
  }
}
