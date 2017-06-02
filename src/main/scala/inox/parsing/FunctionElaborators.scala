package inox.parsing

import inox.ast
import inox.ast.FreshIdentifier

/**
  * Created by junze on 5/18/17.
  */
trait FunctionElaborators { self: Interpolator =>
  trait FunctionElaborator { inner: FunctionIR.type =>

    def getFunctions(fs: List[Function])(symbols: trees.Symbols)
      : (Map[String, ast.Identifier], trees.Symbols) = {
      fs.foldLeft((Map.empty[String, ast.Identifier], symbols)) {
        case ((ids, s), f) => getFunction(f)(ids, s)
      }
    }

    def getFunction(f: Function)(ids: Map[String, ast.Identifier],
                                 symbols: trees.Symbols)
      : (Map[String, ast.Identifier], trees.Symbols) = {
      val funIdentifier = f match {
        case Function(id, _, _, _, _) =>
          FreshIdentifier(id.getName)
      }

      (f match {
        case Function(id, _, _, _, _) =>
          ids + (id.getName -> funIdentifier)
      }, symbols.withFunctions(Seq(f match {
        case Function(id, tParams, args, retType, body) =>
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

          val mapping: Map[String, (inox.Identifier, trees.Type)] = args
            .zip(argIds)
            .map({
              case (Arg(id, tpe), freshID) =>
                id.getName -> (freshID, TypeIR
                  .getTypeWithContext(tpe)(typeParams, symbols.adts))
            })(collection.breakOut)

          val funBody = ExprIR.getExprWithMapping(body, mapping)

          val returnType = retType.fold(funBody.getType(symbols))(
            TypeIR.getTypeWithContext(_)(typeParams, symbols.adts))

          if (returnType != funBody.getType(symbols)) {
            throw new Exception("Wrong return type")
          }

          new trees.FunDef(
            funIdentifier,
            typeParams.values.map(trees.TypeParameterDef(_)).toSeq,
            paramValDef,
            returnType,
            funBody,
            Set.empty)
      })))
    }
  }
}
