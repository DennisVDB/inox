package inox
package parsing

/** Contains abstract Intermediate Representation (IR) language for data-types. */
trait DataTypeIRs extends DataTypeElaborators { self: Interpolator =>
  object DataTypeIR extends IR with DataTypeElaborator {

    type Identifier = String
    type Type = TypeIR.Expression
    type TypeParam = String

    case class DataType(id: Identifier,
                        tps: Seq[TypeParam],
                        constructors: Seq[ValueConstructor])

    case class ValueConstructor(id: Identifier,
                                tps: Seq[TypeParam],
                                args: Seq[Arg])

    case class Arg(id: Identifier, tpe: Type)
  }
}
