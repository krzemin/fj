package fgj.examples

import fgj.AST._
import fgj.Aux._
import fgj.Types._
import erasure.Erasure._

object Lists extends App {

  val Summable = Class("Summable",
    typeParams = List(BoundedParam(TypeVar("X"), ClassType("Object"))),
    baseClass = ClassType("Object"),
    methods = List(Method(
        name = "plus",
        typeParams = List(),
        resultType = TypeVar("X"),
        args = List(Argument("other", TypeVar("X"))),
        body = Var("other")
    ))
  )

  val Nat = Class("Nat", List(), ClassType("Summable", List(ClassType("Nat"))),
    methods = List(
      Method("succ", List(), ClassType("Succ"), List(),
        New(ClassType("Succ"), List(Var("this"))))
    )
  )

  val Zero = Class("Zero", List(), ClassType("Nat"))

  val Succ = Class("Succ", List(), ClassType("Nat"),
    fields = List(
      Field("prev", ClassType("Nat"))
    ),
    methods = List(
      Method("plus", List(), ClassType("Nat"), List(Argument("n", ClassType("Nat"))),
        Invoke(FieldAccess(Var("this"), "prev"), "plus", List(), List(Invoke(Var("n"), "succ"))))
    )
  )

  val UnaryFunc = Class("UnaryFunc",
    List(
      BoundedParam(TypeVar("X"), ClassType("Object")),
      BoundedParam(TypeVar("Y"), ClassType("Object"))
    ),
    ClassType("Object"),
    fields = List(Field("ignored", TypeVar("Y"))),
    methods = List(
      Method("apply", List(), TypeVar("Y"),
        List(Argument("arg", TypeVar("X"))),
        FieldAccess(Var("this"), "ignored"))
    )
  )

  val TwicePlus1 = Class("TwicePlus1", List(),
    ClassType("UnaryFunc", List(ClassType("Nat"), ClassType("Nat"))),
    methods = List(
      Method("apply", List(), ClassType("Succ"),
        List(Argument("n", ClassType("Nat"))),
        Invoke(Invoke(Var("n"), "plus", List(), List(Var("n"))), "succ")
      )
    )
  )

  val Lst = Class("List",
    List(BoundedParam(TypeVar("X"), ClassType("Summable", List(TypeVar("X"))))),
    ClassType("Object"),
    methods = List(
      Method("sum", List(), TypeVar("X"),
        List(Argument("zero", TypeVar("X"))), Var("zero")),
      Method("map",
        List(BoundedParam(TypeVar("Y"), ClassType("Summable", List(TypeVar("Y"))))),
        ClassType("List", List(TypeVar("Y"))),
        List(Argument("f", ClassType("UnaryFunc", List(TypeVar("X"), TypeVar("Y"))))),
        New(ClassType("Nil", List(TypeVar("Y"))))
      )
    )
  )

  val Nill = Class("Nil",
    List(BoundedParam(TypeVar("X"), ClassType("Summable", List(TypeVar("X"))))),
    ClassType("List", List(TypeVar("X")))
  )

  val Cons = Class("Cons",
    List(BoundedParam(TypeVar("X"), ClassType("Summable", List(TypeVar("X"))))),
    ClassType("List", List(TypeVar("X"))),
    fields = List(
      Field("head", TypeVar("X")),
      Field("tail", ClassType("List", List(TypeVar("X"))))
    ),
    methods = List(
      Method("sum", List(), TypeVar("X"),
        List(Argument("zero", TypeVar("X"))),
        Invoke(
          Invoke(FieldAccess(Var("this"), "tail"), "sum", List(), List(Var("zero"))),
          "plus",
          List(),
          List(FieldAccess(Var("this"), "head"))
        )
      ),
      Method("map",
        List(BoundedParam(TypeVar("Y"), ClassType("Summable", List(TypeVar("Y"))))),
        ClassType("List", List(TypeVar("Y"))),
        List(Argument("f", ClassType("UnaryFunc", List(TypeVar("X"), TypeVar("Y"))))),
        New(ClassType("Cons", List(TypeVar("Y"))), List(
          Invoke(Var("f"), "apply", List(), List(FieldAccess(Var("this"), "head"))),
          Invoke(FieldAccess(Var("this"), "tail"), "map", List(), List(Var("f")))
        ))
      )
    )
  )



  val ct = buildClassTable(List(Summable, Nat, Zero, Succ, UnaryFunc, TwicePlus1, Lst, Nill, Cons ))

  ct.values.foreach(println)

  ct.values.foreach { clz =>
    println(s"does class ${clz.name} type? -> ${classTypes(clz.name)(ct, Map.empty, Map.empty)}")
  }


  val zero = New(ClassType("Zero"))
  val one = New(ClassType("Succ"), List(zero))
  val two = New(ClassType("Succ"), List(one))
  val three = New(ClassType("Succ"), List(two))

  val empty = New(ClassType("Nil", List(ClassType("Nat"))))
  val two_ = New(ClassType("Cons", List(ClassType("Nat"))), List(two, empty))
  val three_two_ = New(ClassType("Cons", List(ClassType("Nat"))), List(three, two_))

  val twicePlus1 = New(ClassType("TwicePlus1"), List(zero))

  val p1 = Program(ct, Invoke(three_two_, "sum", List(), List(zero)))
  println(p1.main)
  println(programType(p1))

  val p1e = eraseProgram(p1)
  p1e.classTable.values.foreach(println)

  p1e.classTable.values.foreach { clz =>
    println(s"does class ${clz.name} type? -> ${fj.Types.classTypes(clz.name)(p1e.classTable)}")
  }

  println(p1e.main)
  println(fj.Types.programType(p1e))
  println(fj.Eval.evalProg(p1e))


  val mapped = Invoke(three_two_, "map", List(ClassType("Nat")), List(twicePlus1))

  val p2 = Program(ct, mapped)
  val p2e = eraseProgram(p2)
  println(fj.Types.programType(p2e))
  println(fj.Eval.evalProg(p2e))

  val p3 = Program(ct, Invoke(mapped, "sum", List(), List(zero)))
  val p3e = eraseProgram(p3)
  println(fj.Types.programType(p3e))
  println(fj.Eval.evalProg(p3e))

  val p4 = Program(ct, Invoke(two, "plus", List(), List(one)))
  println(p4.main)
  println(fgj.Types.programType(p4))
  val p4e = eraseProgram(p4)
  println(p4e.main)
  println(fj.Types.programType(p4e))
  println(fj.Eval.evalProg(p4e))

}
