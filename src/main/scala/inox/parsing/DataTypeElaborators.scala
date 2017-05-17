package inox.parsing

import inox.ast
import inox.ast.FreshIdentifier

/**
  * Created by dennis on 15/5/17.
  */
trait DataTypeElaborators { self: Interpolator =>
  trait DataTypeElaborator { inner: DataTypeIR.type =>

    def getDataType(dataType: DataType)(symbols: trees.Symbols): trees.Symbols = {
      dataType match {
        case DataTypeSort(id, tps, constructors) =>
          val adtSortIdentifier = FreshIdentifier(id.name)

          val adtConstructorsIdentifiers: Map[String, ast.Identifier] =
            constructors
              .map(c => c.id.name -> FreshIdentifier(c.id.name))(
                collection.breakOut)

          val identifiers = adtConstructorsIdentifiers + (id.name -> adtSortIdentifier)

          val adtSort = trees.dsl.mkSort(adtSortIdentifier, List.empty: _*)(
            tps: _*)(adtConstructorsIdentifiers.values.toSeq)

          constructors
            .foldLeft(symbols)((s, c) => {
              val adtCons = trees.dsl.mkConstructor(
                adtConstructorsIdentifiers(c.id.name),
                Seq.empty: _*)(c.tps: _*)(Some(adtSortIdentifier)) { _ =>
                c.args.map {
                  case Argument(id, tpe) =>
                    trees.ValDef(identifiers(id.name), TypeIR.getType(tpe))
                }
              }

              s.withADTs(Seq(adtCons))
            })
            .withADTs(Seq(adtSort))
      }
    }
  }
}
