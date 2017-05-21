package inox.parsing

import inox.ast.FreshIdentifier

/**
  * Created by junze on 5/18/17.
  */
trait FunctionElaborators { self: Interpolator =>

  class FunctionElaborator { inner: FunctionIR.type =>

//    def getFunction(f: Function): trees.FunDef = f match {
//      case Function(id, tParams, args, retType, body) =>
//        val funIdentifier = FreshIdentifier(id.getName)
//
//        val typeParams: Map[TypeIR.Value, trees.TypeParameter] = tParams.map(
//          t =>
//            TypeIR.Name(t.name) -> trees
//              .TypeParameter(FreshIdentifier(t.name), t.v match {
//                case Invariant => Set.empty
//                case Covariant => Set(trees.Variance(true))
//                case Contravariant => Set(trees.Variance(false))
//              }))(collection.breakOut)
//
//        trees.dsl.mkFunDef(funIdentifier, Seq.empty: _*)(
//          tParams.map(_.name): _*) { _ =>
//          (args.map {
//            case Arg(id, tpe) =>
//              trees.ValDef(
//                FreshIdentifier(id.getName),
//                TypeIR.getTypeWithContext(tpe)(typeParams, Map.empty))
//          }, TypeIR.getTypeWithContext(retType)(typeParams, Map.empty), _ => ExprIR.getExpr(body))
//        }
//    }

//    def getFunction(function: Function): FunDef = function match {
//      case Function(name, typeParams, arguments, returnType, body) =>
//        val funName = name match {
//          case IdentifierName(n) => FreshIdentifier(n)
//        }
//        val tpNames = typeParams.map(tpId => tpId.getName)
//        val builder =
//
//        val funs = trees.dsl.mkFunDef(funName)(tpNames: _*)(builder)
//
//    }
  }
}
