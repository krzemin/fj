package fgj

object AST {

  type VarName = String
  type TypeName = String
  type TypeVarName = String

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
                    typeArgs: List[Type] = Nil,
                    args: List[Expr] = Nil) extends Expr {
    override def toString =
      s"$expr.$methodName<${typeArgs.mkString(",")}>(${args.mkString(",")})"
  }
  case class New(classType: ClassType,
                 args: List[Expr] = Nil) extends Expr {
    override def toString = s"new $classType(${args.mkString(",")})"
  }
  case class Cast(classType: ClassType,
                  expr: Expr) extends Expr {
    override def toString = s"($classType)$expr"
  }

  trait Type
  case class TypeVar(name: TypeVarName) extends Type {
    override def toString = name
  }
  case class ClassType(className: TypeName,
                       argTypes: List[Type] = Nil) extends Type {
    override def toString = s"$className<${argTypes.map(_.toString).mkString(",")}>"
  }


  case class BoundedParam(typeVar: TypeVar, boundClass: ClassType) {
    override def toString = s"$typeVar extends $boundClass"
  }


  case class Class(name: TypeName,
                   typeParams: List[BoundedParam],
                   baseClass: ClassType,
                   fields: List[Field] = Nil,
                   methods: List[Method] = Nil) {
    override def toString =
      s"class $name<${typeParams.map(_.toString).mkString(",")}> extends $baseClass { " +
        s"${fields.map(_.toString).mkString(" ")} " +
        s"${methods.map(_.toString).mkString(" ")} }"
  }

  case class Field(name: VarName,
                   fieldType: Type) {
    override def toString = {
      s"$fieldType $name;"
    }
  }

  case class Method(name: VarName,
                    typeParams: List[BoundedParam],
                    resultType: Type,
                    args: List[Argument],
                    body: Expr) {
    override def toString =
      s"<${typeParams.map(_.toString).mkString(",")}> $resultType $name" +
      s"(${args.map(_.toString).mkString(",")}) { return $body; }"
  }

  case class Argument(name: VarName,
                      argType: Type) {
    override def toString = {
      s"$argType $name"
    }
  }

  type ClassTable = Map[TypeName, Class]
  case class Program(classTable: ClassTable,
                     main: Expr)
}
