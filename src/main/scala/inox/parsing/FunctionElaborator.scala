package inox.parsing

import inox.ast.FreshIdentifier

/**
  * Created by junze on 5/18/17.
  */
trait FunctionElaborators { self: Interpolator =>

  class FunctionElaborator { inner: FunctionIR.type =>

    def getFunction(function: Function): FunDef = function match {
      case Function(name, typeParams, arguments, returnType, body) =>
        val funName = name match {
          case IdentifierName(n) => FreshIdentifier(n)
        }
        val tpNames = typeParams.map(tpId => tpId.getName)
        val builder =

        val funs = trees.dsl.mkFunDef(funName)(tpNames: _*)(builder)

    }
  }
}
