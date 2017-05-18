package inox.parsing

import inox.ast
import inox.ast.FreshIdentifier

/**
  * Created by dennis on 15/5/17.
  */
trait DataTypeElaborators { self: Interpolator =>
  trait DataTypeElaborator { inner: DataTypeIR.type =>

    def getDataTypes(dataTypes: List[DataType])(
        symbols: trees.Symbols): trees.Symbols = {
      dataTypes.foldLeft(symbols)((s, dt) => getDataType(dt)(s))
    }

    def getDataType(dataType: DataType)(
        symbols: trees.Symbols): trees.Symbols = {
      dataType match {
        case DataType(id, tps, constructors) =>
          val adtSortIdentifier = FreshIdentifier(id)

          val adtConsIdentifiers: Map[Identifier, ast.Identifier] =
            constructors
              .map(c => c.id -> FreshIdentifier(c.id))(collection.breakOut)
          
          val adtSort = trees.dsl.mkSort(adtSortIdentifier, List.empty: _*)(
            tps: _*)(adtConsIdentifiers.values.toSeq)

          constructors
            .foldLeft(symbols)((s, c) => {
              val adtCons =
                trees.dsl.mkConstructor(
                  adtConsIdentifiers(c.id),
                  Seq.empty: _*)(c.tps: _*)(Some(adtSortIdentifier)) { _ =>
                  c.args.map {
                    case Arg(id, tpe) =>
                      trees.ValDef(FreshIdentifier(id), TypeIR.getType(tpe))
                  }
                }

              s.withADTs(Seq(adtCons))
            })
            .withADTs(Seq(adtSort))
      }
    }
  }
}
