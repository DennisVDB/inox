package inox.parsing

import inox.ast.FreshIdentifier

/**
  * Created by junze on 5/18/17.
  */
trait FunctionElaborators { self: Interpolator =>

  class FunctionElaborator { inner: FunctionIR.type =>

    def getFunction(f: Function): trees.FunDef = f match {
      case Function(id, tParams, args, retType, body) =>
        val funIdentifier = FreshIdentifier(id.getName)
        val typeParamsDef = tParams.map(_.getName).map(trees.TypeParameter.fresh).map(trees.TypeParameterDef(_))
        val paramValDef = args.map {
          case Arg(id, tpe) => trees.ValDef(FreshIdentifier(id.getName), TypeIR.getType(tpe), Set.empty)
        }
        val returnType = TypeIR.getType(retType)
        val funBody = ExprIR.getExpr(body)
        new trees.FunDef(funIdentifier, typeParamsDef, paramValDef, returnType, funBody, Set.empty)
    }

  }
}
