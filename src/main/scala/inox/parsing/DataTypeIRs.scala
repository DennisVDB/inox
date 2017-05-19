package inox
package parsing

/** Contains abstract Intermediate Representation (IR) language for data-types. */
trait DataTypeIRs extends DataTypeElaborators { self: Interpolator =>
  object DataTypeIR extends IR with DataTypeElaborator {

    type Identifier = String
    type Type = TypeIR.Expression
    type TypeParam = String

    case class Arg(id: Identifier, tpe: Type)

    case class DataType(id: Identifier,
                        typeParams: Seq[TypeParam],
                        constructors: Seq[ValueConstructor])

    case class ValueConstructor(id: Identifier,
                                typeParams: Seq[TypeParam],
                                args: Seq[Arg])

  }
}
