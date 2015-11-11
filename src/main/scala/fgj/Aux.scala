package fgj

import fgj.AST._
import fgj.Types.MethodType

object Aux {

  def buildClassTable(classes: List[Class]): ClassTable =
    classes.map(c => c.name -> c).toMap

  def isSubclass(c: TypeName, d: TypeName)(cm: ClassTable): Boolean =
    if(c == "Object") d == "Object"
    else c == d || {
      val cBase = cm(c).baseClass.className
      cBase == d || isSubclass(cBase, d)(cm)
    }

  def substituteType(t: Type)(subst: Map[TypeVarName, Type]): Type = t match {
    case TypeVar(x) =>
      subst.getOrElse(x, t)

    case ClassType(className, argTypes) =>
      ClassType(className, argTypes.map(substituteType(_)(subst)))

    case MethodType(typeParams, argTypes, resultType) =>
      MethodType(typeParams,
              argTypes.map(substituteType(_)(subst)),
              substituteType(resultType)(subst))
  }

  def substituteExpr(expr: Expr)(subs: Map[TypeVarName, Type]): Expr = expr match {
    case Var(_) =>
      expr
    case FieldAccess(expr0, f) =>
      FieldAccess(substituteExpr(expr0)(subs), f)
    case Invoke(expr0, methodName, typeArgs, args) =>
      Invoke(substituteExpr(expr0)(subs), methodName, typeArgs.map(substituteType(_)(subs)), args)
    case New(ct, args) =>
      New(substituteType(ct)(subs).asInstanceOf[ClassType], args)
    case Cast(ct, expr0) =>
      Cast(substituteType(ct)(subs).asInstanceOf[ClassType], substituteExpr(expr0)(subs))
  }

  def fields(ct: ClassType)(cm: ClassTable): List[Field] =
    if (ct.className == "Object") Nil
    else {
      val classDef = cm(ct.className)
      if(classDef.typeParams.length != ct.argTypes.length) {
        throw new IllegalArgumentException
      }
      val typeVarNames = classDef.typeParams.map(_.typeVar.name)
      val subst = (typeVarNames zip ct.argTypes).toMap
      val baseClassFields = fields(substituteType(classDef.baseClass)(subst).asInstanceOf[ClassType])(cm)
      val thisClassFields = classDef.fields.map { f =>
        f.copy(fieldType = substituteType(f.fieldType)(subst))
      }
      baseClassFields ++ thisClassFields
    }

  def mtype(methodName: VarName, ct: ClassType)(cm: ClassTable): Option[Type] =
    if(ct.className == "Object") None
    else{
      val typeVarNames = cm(ct.className).typeParams.map(_.typeVar.name)
      val subst = (typeVarNames zip ct.argTypes).toMap
      cm(ct.className).methods.find(_.name == methodName) match {
        case Some(method) =>
          val argTypes = method.args.map(_.argType)
          Some(substituteType(MethodType(method.typeParams, argTypes, method.resultType))(subst))
        case None =>
          mtype(methodName, substituteType(cm(ct.className).baseClass)(subst).asInstanceOf[ClassType])(cm)
      }
    }

  def mbody(methodName: VarName, typeArgs: List[Type], ct: ClassType)(cm: ClassTable): Method =
    cm(ct.className).methods.find(_.name == methodName) match {
      case Some(method) =>
        val classTypeVarNames = cm(ct.className).typeParams.map(_.typeVar.name)
        val classSubst = (classTypeVarNames zip ct.argTypes).toMap
        val methodTypeVarNames = method.typeParams.map(_.typeVar.name)
        val methodSubst = (methodTypeVarNames zip typeArgs).toMap
        val e0 = substituteExpr(method.body)(classSubst)
        val e1 = substituteExpr(e0)(methodSubst)
        val r0 = substituteType(method.resultType)(classSubst)
        val r1 = substituteType(r0)(methodSubst)
        val args1 = method.args.map { arg =>
          val t0 = substituteType(arg.argType)(classSubst)
          arg.copy(argType = substituteType(t0)(methodSubst))
        }
        method.copy(body = e1, resultType = r1, args = args1)
      case None =>
        val baseClassName = cm(ct.className).baseClass
        mbody(methodName, typeArgs, baseClassName)(cm)
    }

}
