package fj

import scala.annotation.tailrec

object Types {
  import fj.AST._
  import fj.Aux._

  trait Type
  case class SimpleType(name: TypeName) extends Type {
    override def toString = name
  }
  case class MethodType(argTypes: List[SimpleType],
                        resultType: SimpleType) extends Type {
    override def toString = {
      val argTypesStr = argTypes.map(_.toString).mkString(",")
      s"($argTypesStr) -> $resultType"
    }
  }

  @tailrec
  def isSubtype(t: TypeName, u: TypeName)(ct: ClassTable): Boolean =
    if(t == "Object") u == "Object"
    else (t == u) || {
      val tBase = ct(t).baseClassName
      isSubtype(tBase, u)(ct)
    }

  def areSubtypes(ts: List[TypeName], us: List[TypeName])(ct: ClassTable): Boolean =
    ts.length == us.length && (ts zip us).forall {
      case (t, u) => isSubtype(t, u)(ct)
    }

  type Γ = Map[VarName, SimpleType]

  def exprType(e: Expr)(g: Γ, ct: ClassTable): Option[SimpleType] = e match {
    case Var(name) =>
      g.get(name)

    case FieldAccess(e0, fieldName) => for {
        SimpleType(c0) <- exprType(e0)(g, ct)
        fieldsC0 = fields(c0)(ct)
        field <- fieldsC0.find(_.name == fieldName)
      } yield SimpleType(field.fieldType)

    case Invoke(e0, m, args) => for {
        SimpleType(c0) <- exprType(e0)(g, ct)
        MethodType(paramTypes, resType) <- mtype(m, c0)(ct)
        argTypes <- exprType(args.toList)(g, ct)
        if areSubtypes(argTypes.map(_.name), paramTypes.map(_.name))(ct)
      } yield resType

    case New(c, args) => for {
        argTypes <- exprType(args.toList)(g, ct)
        fieldsC = fields(c)(ct)
        if areSubtypes(argTypes.map(_.name), fieldsC.map(_.fieldType))(ct)
      } yield SimpleType(c)

    case Cast(c, e0) => for {
        SimpleType(d) <- exprType(e0)(g, ct)
      } yield {
        if(!isSubtype(c, d)(ct) && !isSubtype(d, c)(ct)) {
          println(s"stupid warning: ($c) ($e0 : $d)")
        }
        SimpleType(c)
      }
  }

  def exprType(es: List[Expr])(g: Γ, ct: ClassTable): Option[List[SimpleType]] =
    es.map(exprType(_)(g, ct)) match {
      case lst if lst.contains(None) => None
      case lst => Some(lst.flatten)
    }

  def methodTypes(className: TypeName, name: VarName)(ct: ClassTable): Boolean = {
    val method = ct(className).methods.find(_.name == name).head
    val mParams = method.args.map(arg => arg.name -> SimpleType(arg.argType))
    val newG = mParams.toMap + ("this" -> SimpleType(className))
    val d = ct(className).baseClassName
    exprType(method.body)(newG, ct) match {
      case None =>
        false
      case Some(SimpleType(eType)) =>
        isSubtype(eType, method.resultType)(ct) && (mtype(name, d)(ct) match {
          case None =>
            true
          case Some(MethodType(ds, SimpleType(d0))) =>
            ds.map(_.name) == method.args.map(_.argType) && method.resultType == d0
        })
    }
  }

  def classTypes(c: TypeName)(cm: ClassTable): Boolean =
    cm(c).methods.forall(method => methodTypes(cm(c).name, method.name)(cm))

  def programType(p: Program): Option[Type] = {
    p.classTable.values.forall(c => classTypes(c.name)(p.classTable)) match {
      case false =>
        None
      case true =>
        exprType(p.main)(Map.empty, p.classTable)
    }
  }


}
