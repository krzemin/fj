package fj

object Eval {

  import AST._
  import Aux._

  case class Value(className: TypeName, args: List[Value] = Nil) {
    override def toString = {
      val vsStr = args.map(_.toString).mkString(",")
      s"new $className($vsStr)"
    }
  }

  type Env = Map[VarName, Value]

  def eval(expr: Expr)(env: Env, cm: ClassTable): Value = expr match {

    case Var(x) =>
      env(x)

    case FieldAccess(expr0, fieldName) =>
      val Value(className, args) = eval(expr0)(env, cm)
      val allFields = fields(className)(cm)
      (allFields zip args)
        .collect {
          case (field, argument) if field.name == fieldName => argument
        }
        .headOption
        .getOrElse {
          throw new RuntimeException(s"field $fieldName not present in class $className")
        }

    case Invoke(expr0, methodName, args) =>
      val thisObj @ Value(thisClass, _) = eval(expr0)(env, cm)
      val argObjs = args.map(eval(_)(env, cm))
      val method = mbody(methodName, thisClass)(cm)
      val argNames = method.args.map(_.name)
      val newEnv = env ++ (argNames zip argObjs) + ("this" -> thisObj)
      eval(method.body)(newEnv, cm)

    case New(className, args) =>
      Value(className, args.map(eval(_)(env, cm)))

    case Cast(destClassName, expr0) =>
      val obj @ Value(className, _) = eval(expr0)(env, cm)
      if(Types.isSubtype(className, destClassName)(cm)) obj
      else throw new RuntimeException(s"Cast error from $className to $destClassName")

  }

  def evalProg(prog: Program): Value = {
    eval(prog.main)(Map.empty, prog.classTable)
  }

}
