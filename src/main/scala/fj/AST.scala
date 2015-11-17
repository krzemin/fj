package fj

object AST {
  type VarName = String
  type TypeName = String

  trait Expr

  case class Var(name: VarName) extends Expr {
    override def toString = name
  }
  case class FieldAccess(expr: Expr,
                         fieldName: VarName) extends Expr {
    override def toString = s"$expr.$fieldName"
  }
  case class Invoke(expr: Expr,
                    methodName: VarName,
                    args: List[Expr] = Nil) extends Expr {
    override def toString =
      s"$expr.$methodName(${args.map(_.toString).mkString(",")})"
  }
  case class New(className: TypeName,
                 args: List[Expr] = Nil) extends Expr {
    override def toString =
      s"new $className(${args.map(_.toString).mkString(",")})"
  }
  case class Cast(className: TypeName,
                  expr: Expr) extends Expr {
    override def toString =
      s"($className)($expr)"
  }

  case class Class(name: TypeName,
                   baseClass: TypeName,
                   fields: List[Field] = Nil,
                   methods: List[Method] = Nil) {
    override def toString =
      s"class $name extends $baseClass { " +
        s"${fields.map(_.toString).mkString(" ")} " +
        s"${methods.map(_.toString).mkString(" ")} }"
  }

  case class Field(name: VarName,
                   fieldType: TypeName) {
    override def toString = {
      s"$fieldType $name;"
    }
  }

  case class Method(name: VarName,
                    resultType: TypeName,
                    args: List[Argument],
                    body: Expr) {
    override def toString = {
      s"$resultType $name(${args.map(_.toString).mkString(",")}) { return $body; }"
    }
  }

  case class Argument(name: VarName,
                      argType: TypeName) {
    override def toString = {
      s"$argType $name"
    }
  }

  type ClassTable = Map[TypeName, Class]

  case class Program(classTable: ClassTable,
                     main: Expr)


}
