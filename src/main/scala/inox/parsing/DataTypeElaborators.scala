package inox.parsing

import inox.ast
import inox.ast.FreshIdentifier

/**
  * Created by dennis on 15/5/17.
  */
trait DataTypeElaborators { self: Interpolator =>
  trait DataTypeElaborator { inner: DataTypeIR.type =>

    def getDataTypes(dataTypes: List[DataType]): trees.Symbols = {
      dataTypes.foldLeft(symbols)((s, dt) => getDataType(dt)(s))
    }

    def getDataType(dataType: DataType)(
        symbols: trees.Symbols): trees.Symbols = {
      dataType match {
        case DataType(id, tParams, constructors) =>
          val adtSortIdentifier = FreshIdentifier(id)

          val adtConsIdentifiers: Map[Identifier, ast.Identifier] =
            constructors
              .map(c => c.id -> FreshIdentifier(c.id))(collection.breakOut)

          val adtSort = trees.dsl.mkSort(adtSortIdentifier, List.empty: _*)(
            tParams: _*)(adtConsIdentifiers.values.toSeq)

          val typeParams: Map[TypeIR.Value, trees.TypeParameter] = tParams.map(
            t =>
              TypeIR.Name(t) -> trees.TypeParameter(FreshIdentifier(t),
                                                    Set.empty))(
            collection.breakOut)

          constructors
            .foldLeft(symbols.withADTs(Seq(adtSort)))((s, c) => {
              val adtCons =
                trees.dsl.mkConstructor(
                  adtConsIdentifiers(c.id),
                  Seq.empty: _*)(c.typeParams: _*)(Some(adtSortIdentifier)) {
                  _ =>
                    c.args.map {
                      case Arg(id, tpe) =>
                        trees.ValDef(
                          FreshIdentifier(id),
                          TypeIR.getTypeWithContext(tpe)(typeParams, s.adts))
                    }
                }

              s.withADTs(Seq(adtCons))
            })
      }
    }
  }
}
