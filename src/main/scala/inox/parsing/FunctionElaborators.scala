package inox.parsing

import inox.ast.FreshIdentifier

/**
  * Created by junze on 5/18/17.
  */
trait FunctionElaborators { self: Interpolator =>
  trait FunctionElaborator { inner: FunctionIR.type =>

    def getFunctions(fs: List[Function])(
        symbols: trees.Symbols): trees.Symbols = {
      fs.foldLeft(symbols)((s, f) => getFunction(f)(s))
    }

    def getFunction(f: Function)(symbols: trees.Symbols): trees.Symbols =
      symbols.withFunctions(Seq(f match {
        case Function(id, tParams, args, retType, body) =>
          val funIdentifier = FreshIdentifier(id.getName)

          val argIds = args.map {
            case Arg(id, _) => FreshIdentifier(id.getName)
          }

          val typeParams: Map[TypeIR.Value, trees.TypeParameter] = tParams.map(
            t => TypeIR.Name(t.getName) -> trees.TypeParameter.fresh(t.getName)
          )(collection.breakOut)

          val paramValDef = args zip argIds map {
            case (Arg(id, tpe), freshID) =>
              trees.ValDef(
                freshID,
                TypeIR.getTypeWithContext(tpe)(typeParams, symbols.adts),
                Set.empty)
          }

          val returnType =
            TypeIR.getTypeWithContext(retType)(typeParams, symbols.adts)

          val mapping: Map[String, (inox.Identifier, trees.Type)] = args
            .zip(argIds)
            .map({
              case (Arg(id, tpe), freshID) =>
                id.getName -> (freshID, TypeIR
                  .getTypeWithContext(tpe)(typeParams, symbols.adts))
            })(collection.breakOut)

          val funBody = ExprIR.getExprWithMapping(body, mapping)

          new trees.FunDef(
            funIdentifier,
            typeParams.values.map(trees.TypeParameterDef(_)).toSeq,
            paramValDef,
            returnType,
            funBody,
            Set.empty)
      }))

  }
}
