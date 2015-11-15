package fgj

import fgj.AST._
import fgj.Aux._

object Types {

  type Γ = Map[VarName, Type]
  type Δ = Map[TypeVarName, ClassType]

  case class MethodType(typeParams: List[BoundedParam],
                        argTypes: List[Type],
                        resultType: Type) extends Type {
    override def toString = {
      val typeParamsStr = typeParams.map(_.toString).mkString(",")
      val argTypesStr = argTypes.map(_.toString).mkString(",")
      s"<$typeParamsStr> ($argTypesStr) -> $resultType"
    }
  }

  def bound(t: Type)(d: Δ): ClassType = t match {
    case TypeVar(name) => d(name)
    case u: ClassType => u
  }

  def isSubtype(t: Type, u: Type)(ct: ClassTable, d: Δ): Boolean = {
    if (t == u) true
    else t match {
      case TypeVar(tName) =>
        isSubtype(d(tName), u)(ct, d)

      case ClassType(cName, typeArgs) if ct.contains(cName) =>
        val classDef = ct(cName)
        val subst = classDef.typeParams
          .map(_.typeVar.name)
          .zip(typeArgs)
          .toMap
        val t1 = substituteType(classDef.baseClass)(subst)
        isSubtype(t1, u)(ct, d)

      case _ =>
        false
    }
  }

  def isTypeWellFormed(t: Type)(ct: ClassTable, d: Δ): Boolean = t match {
    case TypeVar(x) =>
      d.contains(x)

    case ClassType("Object", args) =>
      args.isEmpty

    case ClassType(c, args) =>
      val subst = (ct(c).typeParams.map(_.typeVar.name) zip args).toMap
      val superTypes = ct(c).typeParams.map(_.boundClass).map(substituteType(_)(subst))
      val argsAreSubtypes = args.zip(superTypes).forall {
        case (t1, u1) =>
          isSubtype(t1, u1)(ct, d)
      }
      args.forall(isTypeWellFormed(_)(ct, d)) && argsAreSubtypes
  }

  def fv(t: Type): Set[TypeVar] = t match {
    case x: TypeVar => Set(x)
    case ClassType(_, args) =>
      args.flatMap(fv).toSet
  }

  def dcast(c: ClassType, d: ClassType)(ct: ClassTable): Boolean = {
    val classC = ct(c.className)
    val typeVarsSet = classC.typeParams.map(_.typeVar).toSet
    val baseClassFreeVarsSet = fv(classC.baseClass)

    typeVarsSet == baseClassFreeVarsSet &&
      (classC.baseClass.className == d.className ||
        dcast(classC.baseClass, d)(ct))
  }

  def validOverride(m: VarName, classT: ClassType, ft: MethodType)(ct: ClassTable): Boolean =
    mtype(m, classT)(ct) match {
      case Some(MethodType(mTypeParams, mParams, mResult)) =>
        val MethodType(fTypeParams, fParams, fResult) = ft
        val subst = (mTypeParams.map(_.typeVar.name) zip fTypeParams.map(_.typeVar)).toMap
        val mBounds = mTypeParams.map(_.boundClass)
        val fBounds = fTypeParams.map(_.boundClass)
        val d = fTypeParams.map(e => e.typeVar.name -> e.boundClass).toMap
        mBounds.map(substituteType(_)(subst)) == fBounds &&
          mParams.map(substituteType(_)(subst)) == fParams &&
          isSubtype(fResult, substituteType(mResult)(subst))(ct, d)
      case Some(_) =>
        false
      case None =>
        true
    }

  def exprType(e: Expr)(ct: ClassTable, g: Γ, d: Δ): Option[Type] = e match {
    case Var(x) =>
      g.get(x)

    case FieldAccess(expr0, fieldName) => for {
      t0 <- exprType(expr0)(ct, g, d)
      t0Bound = bound(t0)(d)
      fieldsT0 = fields(t0Bound)(ct)
      field <- fieldsT0.find(_.name == fieldName)
    } yield field.fieldType

    case Invoke(e0, m, tArgs, args) => for {
      t0 <- exprType(e0)(ct, g, d)
      MethodType(mTypeParams, mParams, mResult) <- mtype(m, bound(t0)(d))(ct)
      if tArgs.forall(isTypeWellFormed(_)(ct, d))
      subst = (mTypeParams.map(_.typeVar.name) zip tArgs).toMap
      if tArgs.zip(mTypeParams.map(_.typeVar)).forall {
        case (t1, u1) =>
          isSubtype(t1, substituteType(u1)(subst))(ct, d)
      }
      argTypes <- exprType(args.toList)(ct, g, d)
      if (argTypes zip mParams).forall {
        case (t1, u1) =>
          isSubtype(t1, substituteType(u1)(subst))(ct, d)
      }
    } yield substituteType(mResult)(subst)

    case New(classT, args) => for {
      es <- exprType(args.toList)(ct, g, d)
      fts = fields(classT)(ct).map(_.fieldType)
      if isTypeWellFormed(classT)(ct, d)
      if (es zip fts).forall { case (t1, u1) => isSubtype(t1, u1)(ct, d) }
    } yield classT

    case Cast(classT, e0) => for {
      t0 <- exprType(e0)(ct, g, d)
      t0b = bound(t0)(d)
      if {
        isSubtype(t0b, classT)(ct, d) ||
          isTypeWellFormed(classT)(ct, d) && {
            {
              isSubtype(classT, t0b)(ct, d) &&
                dcast(classT, t0b)(ct)
            } || {
              !isSubclass(classT.className, t0b.className)(ct) &&
                !isSubclass(t0b.className, classT.className)(ct) && {
                println(s"stupid warning: ($ct) ($e0 : $t0b)")
                true
              }
            }
          }
      }
    } yield classT
  }

  def exprType(es: List[Expr])(ct: ClassTable, g: Γ, d: Δ): Option[List[Type]] =
    es.map(exprType(_)(ct, g, d)) match {
      case lst if lst.contains(None) => None
      case lst => Some(lst.flatten)
    }

  def methodTypes(classT: ClassType, methodName: VarName)(ct: ClassTable, g: Γ, d: Δ): Boolean = {
    val method = ct(classT.className).methods.find(_.name == methodName).head
    val d1 = (ct(classT.className).typeParams ++ method.typeParams)
      .map(e => e.typeVar.name -> e.boundClass).toMap
    val g1 = method.args.map(a => a.name -> a.argType).toMap + ("this" -> classT)
    exprType(method.body)(ct, g1, d1) match {
      case None =>
        false
      case Some(bodyType) =>
        val argTypes = method.args.map(_.argType)
        val resultTypeIsSubtypeOfDeclared = isSubtype(bodyType, method.resultType)(ct, d1)
        val resultTypeOk = isTypeWellFormed(method.resultType)(ct, d1)
        val argTypesOk = argTypes.forall(isTypeWellFormed(_)(ct, d1))
        val typeParamsOk = method.typeParams.map(_.boundClass).forall(isTypeWellFormed(_)(ct, d1))
        val overrideValid = validOverride(methodName, ct(classT.className).baseClass,
          MethodType(method.typeParams, argTypes, method.resultType))(ct)

        resultTypeIsSubtypeOfDeclared && resultTypeOk && argTypesOk && typeParamsOk && overrideValid
    }
  }

  def classTypes(c: TypeName)(ct: ClassTable, g: Γ, d: Δ): Boolean = {
    val classDef = ct(c)
    val d1 = classDef.typeParams.map(e => e.typeVar.name -> e.boundClass).toMap
    val classT = ClassType(c, classDef.typeParams.map(_.typeVar))
    val allMethodsOk = classDef.methods.forall(m => methodTypes(classT, m.name)(ct, g, d))
    val baseClassOk = isTypeWellFormed(classDef.baseClass)(ct, d1)
    val typeParamsOk = classDef.typeParams.map(_.boundClass).forall(isTypeWellFormed(_)(ct, d1))
    val fieldsOk = classDef.fields.map(_.fieldType).forall(isTypeWellFormed(_)(ct, d1))
    allMethodsOk && baseClassOk && typeParamsOk && fieldsOk
  }

  def programType(p: Program): Option[Type] = {
    val g: Γ = Map.empty
    val d: Δ = Map.empty

    p.classTable.values.forall(c => classTypes(c.name)(p.classTable, g, d)) match {
      case false => None
      case true => exprType(p.main)(p.classTable, g, d)
    }
  }

}
