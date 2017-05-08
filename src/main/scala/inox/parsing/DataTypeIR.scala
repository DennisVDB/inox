package inox
package parsing

import scala.util.parsing.input.Positional

/** Contains abstract Intermediate Representation (IR) language for data-types. */
object DataTypeIR { self: Interpolator =>

  case class Identifier(name: String)

  type Type = String

  abstract class DataType(pre: String) extends Positional with Product {
    override def productPrefix: String = pos + "@" + pre
  }

  case class TypeConstructor(id: Identifier,
                             tps: Seq[Type],
                             constructors: Seq[DataConstructor])
      extends DataType("TypeConstructor")

  case class DataConstructor(id: Identifier,
                             tps: Seq[Type],
                             args: Seq[(Identifier, Type)])
      extends DataType("DataConstructor")
}
