package erasure

import fj.{AST => FJ}
import fgj.{AST => FGJ}
import fgj.Types.{Δ, Γ, bound, exprType}
import fgj.Aux.mtype

object Erasure {

  def fieldsmax(c: FGJ.TypeName)(cm: FGJ.ClassTable): List[(FGJ.VarName, FGJ.TypeName)] =
    if (c == "Object") Nil
    else {
      val classDef = cm(c)
      val d = classDef.typeParams.map(e => e.typeVar.name -> e.bound).toMap
      fieldsmax(classDef.baseClass.className)(cm) ++ classDef.fields.map { f =>
        f.name -> eraseType(f.fieldType)(d)
      }
    }

  def mtypemax(methodName: FGJ.VarName, className: FJ.TypeName)(cm: FGJ.ClassTable): fj.Types.MethodType = {
    val classDef = cm(className)
    mtype(methodName, classDef.baseClass)(cm) match {
      case Some(t) =>
        mtypemax(methodName, classDef.baseClass.className)(cm)
      case None =>
        val method = classDef.methods.find(_.name == methodName).head
        val d = classDef.typeParams.map(e => e.typeVar.name -> e.bound).toMap ++
          method.typeParams.map(e => e.typeVar.name -> e.bound).toMap
        val argTypes = method.args.map(arg => eraseType(arg.argType)(d)).map(fj.Types.SimpleType)
        val resultType = eraseType(method.resultType)(d)
        fj.Types.MethodType(argTypes, fj.Types.SimpleType(resultType))
    }
  }

  def eraseType(t: FGJ.Type)(d: Δ): FJ.TypeName =
    bound(t)(d).className

  def eraseExpr(expr: FGJ.Expr)(cm: FGJ.ClassTable, g: Γ, d: Δ): FJ.Expr = expr match {
    case FGJ.Var(x) =>
      FJ.Var(x)
    case FGJ.FieldAccess(expr0, fieldName) => (for {
        t <- exprType(expr)(cm, g, d)
        t0 <- exprType(expr0)(cm, g, d)
        tErasure = eraseType(t)(d)
        t0Erasure = eraseType(t0)(d)
        fmt0 = fieldsmax(t0Erasure)(cm)
        field <- fmt0.find { case (x, _) => x == fieldName }
        fieldAccess = FJ.FieldAccess(eraseExpr(expr0)(cm, g, d), fieldName)
      } yield field._2 == tErasure match {
        case true => fieldAccess
        case false => FJ.Cast(tErasure, fieldAccess)
      }).getOrElse(throw new IllegalArgumentException)
    case FGJ.Invoke(expr0, m, tArgs, args) => (for {
        t <- exprType(expr)(cm, g, d)
        t0 <- exprType(expr0)(cm, g, d)
        tErasure = eraseType(t)(d)
        t0Erasure = eraseType(t0)(d)
        fj.Types.MethodType(_, fj.Types.SimpleType(resultType)) = mtypemax(m, t0Erasure)(cm)
        invoke = FJ.Invoke(eraseExpr(expr0)(cm, g, d), m, args.map(e1 => eraseExpr(e1)(cm, g, d)))
      } yield resultType == tErasure match {
        case true => invoke
        case false => FJ.Cast(tErasure, invoke)
      }).getOrElse(throw new IllegalArgumentException)
    case FGJ.New(ct, args) =>
      FJ.New(eraseType(ct)(d), args.map(eraseExpr(_)(cm, g, d)))
    case FGJ.Cast(ct, expr1) =>
      FJ.Cast(eraseType(ct)(d), eraseExpr(expr1)(cm, g, d))
  }

  def substExpr(expr: FJ.Expr)(subst: Map[FJ.VarName, FJ.Expr]): FJ.Expr = expr match {
    case FJ.Var(x) =>
      subst.getOrElse(x, expr)
    case FJ.FieldAccess(e0, f) =>
      FJ.FieldAccess(substExpr(e0)(subst), f)
    case FJ.Invoke(e0, m, args) =>
      FJ.Invoke(substExpr(e0)(subst), m, args.map(substExpr(_)(subst)))
    case FJ.New(c, args) =>
      FJ.New(c, args.map(substExpr(_)(subst)))
    case FJ.Cast(c, e0) =>
      FJ.Cast(c, substExpr(e0)(subst))
  }

  def eraseMethod(classType: FGJ.ClassType, method: FGJ.Method)(cm: FGJ.ClassTable, d: Δ): FJ.Method = {
    val g: Γ = method.args.map(a => a.name -> a.argType).toMap + ("this" -> classType)
    val d1 = d ++ method.typeParams.map(e => e.typeVar.name -> e.bound)
    import fj.Types._
    val MethodType(argTypes, SimpleType(resultType)) = mtypemax(method.name, classType.className)(cm)
    val args = method.args.zip(argTypes).map {
      case (fgj.AST.Argument(x, _), SimpleType(tName)) =>
        fj.AST.Argument(s"$x'", tName)
    }
    val subst = method.args.zip(argTypes).map {
      case (fgj.AST.Argument(x, t), SimpleType(typeName)) =>
        typeName == eraseType(t)(d1) match {
          case true => x -> FJ.Var(s"$x'")
          case false => x -> FJ.Cast(eraseType(t)(d1), FJ.Var(s"$x'"))
        }
    }.toMap
    val ee = eraseExpr(method.body)(cm, g, d1)
    FJ.Method(method.name, resultType, args, substExpr(ee)(subst))
  }

  def eraseClass(c: FGJ.Class)(cm: FGJ.ClassTable): FJ.Class = c match {
    case FGJ.Class(className, typeParams, parent, fields, methods) =>
      val ct = FGJ.ClassType(className, typeParams.map(_.typeVar))
      val d = typeParams.map(e => e.typeVar.name -> e.bound).toMap
      val eFields = fields.map(f => fj.AST.Field(f.name, eraseType(f.fieldType)(d)))
      val eMethods = methods.map(eraseMethod(ct, _)(cm, d))
      val eParent = eraseType(parent)(d)
      FJ.Class(className, eParent, eFields, eMethods)
  }

  def eraseProgram(p: FGJ.Program): FJ.Program = {
    val erasedCT = p.classTable.mapValues(eraseClass(_)(p.classTable))
    val erasedMain = eraseExpr(p.main)(p.classTable, Map.empty, Map.empty)
    FJ.Program(erasedCT, erasedMain)
  }


}
