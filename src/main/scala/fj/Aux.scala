package fj

object Aux {
  import fj.AST._
  import fj.Types._

  def buildClassTable(classes: List[Class]): ClassTable =
    classes.map(c => c.name -> c).toMap

  def fields(className: TypeName)(cm: ClassTable): List[Field] =
    if (className == "Object") Nil
    else {
      val classDef = cm(className)
      fields(classDef.baseClassName)(cm) ++ classDef.fields
    }

  def mtype(methodName: VarName, className: TypeName)(cm: ClassTable): Option[MethodType] =
    if (className == "Object") None
    else cm(className).methods.find(_.name == methodName) match {
      case Some(method) =>
        val argTypes = method.args.map(_.argType).map(SimpleType)
        val resType = SimpleType(method.resultType)
        Some(MethodType(argTypes, resType))
      case None =>
        mtype(methodName, cm(className).baseClassName)(cm)
    }

  def mbody(methodName: VarName, c: TypeName)(cm: ClassTable): Method =
    cm(c).methods.find(_.name == methodName) match {
      case Some(method) =>
        method
      case None =>
        mbody(methodName, cm(c).baseClassName)(cm)
    }

}
