package fgj

import fgj.AST._
import fgj.Aux._

import scala.annotation.tailrec

object Types {

  type Γ = Map[VarName, Type]
  type Δ = Map[TypeVarName, Type]

  case class MethodType(typeParams: List[BoundedParam],
                        argTypes: List[Type],
                        resultType: Type) extends Type {
    override def toString = {
      val typeParamsStr = typeParams.map(_.toString).mkString(",")
      val argTypesStr = argTypes.map(_.toString).mkString(",")
      s"<$typeParamsStr> ($argTypesStr) -> $resultType"
    }
  }

  @tailrec
  def bound(t: Type)(d: Δ): ClassType = t match {
    case TypeVar(name) => bound(d(name))(d)
    case u: ClassType => u
  }

  def isSubtype(t: Type, u: Type)(cm: ClassTable, d: Δ): Boolean = {
    lazy val sRefl: Boolean = t == u
    lazy val sVar: Boolean = t match {
      case TypeVar(name) =>
        isSubtype(d(name), u)(cm, d)
      case _ =>
        false
    }
    lazy val sClass: Boolean = t match {
      case ClassType(c, typeArgs) =>
        val classDef = cm(c)
        val subst = classDef.typeParams
          .map(_.typeVar.name)
          .zip(typeArgs)
          .toMap
        u == substituteType(classDef.baseClass)(subst)
      case _ => false
    }
    sRefl || sVar || sClass
  }

  def isTypeWellFormed(t: Type)(cm: ClassTable, d: Δ): Boolean = t match {
    case TypeVar(x) =>
      d.contains(x)

    case ClassType(c, args) =>
      lazy val isObject = c == "Object" && args.isEmpty
      lazy val isWFClass = {
        val subst = (cm(c).typeParams.map(_.typeVar.name) zip args).toMap
        val superTypes = cm(c).typeParams.map(_.bound).map(substituteType(_)(subst))
        val argsAreSubtypes = args.zip(superTypes).forall {
          case (t1, u1) => isSubtype(t1, u1)(cm, d)
        }
        args.forall(isTypeWellFormed(_)(cm, d)) && argsAreSubtypes
      }
      isObject || isWFClass
  }

  def fv(t: Type): Set[TypeVar] = t match {
    case x: TypeVar => Set(x)
    case ClassType(_, args) =>
      args.flatMap(fv).toSet
  }

  def dcast(c: ClassType, d: ClassType)(cm: ClassTable): Boolean = {
    val classC = cm(c.className)
    classC.typeParams.map(_.typeVar).toSet == fv(classC.baseClass) &&
      (classC.baseClass.className == d.className || dcast(classC.baseClass, d)(cm))
  }

  def validOverride(m: VarName, ct: ClassType, ft: MethodType)(cm: ClassTable): Boolean =
    mtype(m, ct)(cm) match {
      case Some(MethodType(mTypeParams, mParams, mResult)) =>
        val MethodType(fTypeParams, fParams, fResult) = ft
        val subst = (mTypeParams.map(_.typeVar.name) zip fTypeParams.map(_.typeVar)).toMap
        val mBounds = mTypeParams.map(_.bound)
        val fBounds = fTypeParams.map(_.bound)
        val d = fTypeParams.map(e => e.typeVar.name -> e.bound).toMap
        mBounds.map(substituteType(_)(subst)) == fBounds &&
          mParams.map(substituteType(_)(subst)) == fParams &&
          isSubtype(fResult, substituteType(mResult)(subst))(cm, d)
      case Some(_) =>
        false
      case None =>
        true
    }

  def exprType(e: Expr)(cm: ClassTable, g: Γ, d: Δ): Option[Type] = e match {
    case Var(x) =>
      g.get(x)

    case FieldAccess(expr0, fieldName) => for {
      t0 <- exprType(expr0)(cm, g, d)
      fieldsT0 = fields(bound(t0)(d))(cm)
      field <- fieldsT0.find(_.name == fieldName)
    } yield field.fieldType

    case Invoke(e0, m, tArgs, args) => for {
      t0 <- exprType(e0)(cm, g, d)
      MethodType(mTypeParams, mParams, mResult) <- mtype(m, bound(t0)(d))(cm)
      if tArgs.forall(isTypeWellFormed(_)(cm, d))
      subst = (mTypeParams.map(_.typeVar.name) zip tArgs).toMap
      if tArgs.zip(mTypeParams.map(_.typeVar)).forall {
        case (t1, u1) => isSubtype(t1, substituteType(u1)(subst))(cm, d)
      }
      argTypes <- exprType(args.toList)(cm, g, d)
      if (argTypes zip mParams).forall {
        case (t1, u1) => isSubtype(t1, substituteType(u1)(subst))(cm, d)
      }
    } yield substituteType(mResult)(subst)

    case New(ct, args) => for {
      es <- exprType(args.toList)(cm, g, d)
      fts = fields(ct)(cm).map(_.fieldType)
      if isTypeWellFormed(ct)(cm, d)
      if (es zip fts).forall { case (t1, u1) => isSubtype(t1, u1)(cm, d) }
    } yield ct

    case Cast(ct, e0) => for {
      t0 <- exprType(e0)(cm, g, d)
      t0b = bound(t0)(d)
      if {
        isSubtype(t0b, ct)(cm, d) ||
        isTypeWellFormed(ct)(cm, d) && {
          {
            isSubtype(ct, t0b)(cm, d) &&
            dcast(ct, t0b)(cm)
          } ||
          {
            !isSubclass(ct.className, t0b.className)(cm) &&
            !isSubclass(t0b.className, ct.className)(cm) && {
              println(s"stupid warning: ($ct) ($e0 : $t0b)")
              true
            }
          }
        }
      }
    } yield ct
  }

  def exprType(es: List[Expr])(cm: ClassTable, g: Γ, d: Δ): Option[List[Type]] =
    es.map(exprType(_)(cm, g, d)) match {
      case lst if lst.contains(None) => None
      case lst => Some(lst.flatten)
    }

  def methodTypes(ct: ClassType, methodName: VarName)(cm: ClassTable, g: Γ, d: Δ): Boolean = {
    val method = cm(ct.className).methods.find(_.name == methodName).head
    val d1 = (cm(ct.className).typeParams ++ method.typeParams)
      .map(e => e.typeVar.name -> e.bound).toMap
    val g1 = method.args.map(a => a.name -> a.argType).toMap + ("this" -> ct)
    exprType(method.body)(cm, g1, d1) match {
      case None =>
        false
      case Some(s) =>
        val argTypes = method.args.map(_.argType)
        isSubtype(s, method.resultType)(cm, d1) &&
          (method.resultType :: argTypes ++ method.typeParams.map(_.bound))
            .forall(isTypeWellFormed(_)(cm, d1)) &&
          validOverride(methodName, cm(ct.className).baseClass,
            MethodType(method.typeParams, argTypes, method.resultType))(cm)
    }
  }

  def classTypes(c: TypeName)(cm: ClassTable, g: Γ, d: Δ): Boolean = {
    val classDef = cm(c)
    val d1 = classDef.typeParams.map(e => e.typeVar.name -> e.bound).toMap
    val ct = ClassType(c, classDef.typeParams.map(_.typeVar))
    classDef.methods.forall(m => methodTypes(ct, m.name)(cm, g, d)) &&
      (classDef.baseClass :: classDef.typeParams.map(_.bound) ++ classDef.fields.map(_.fieldType))
        .forall(isTypeWellFormed(_)(cm, d1))
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
