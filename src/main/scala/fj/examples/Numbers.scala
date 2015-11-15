package fj.examples

import fj.AST._
import fj.Aux._
import fj.Types._
import fj.Eval._

object Numbers extends App {

  val Nat = Class("Nat", "Object",
    fields = List(),
    methods = List(
      Method("succ", "Nat", List(), New("Succ", List(Var("this"))))
    )
  )

  val Zero = Class("Zero", "Nat",
    methods = List(
      Method("plus", "Nat", List(Argument("n", "Nat")), Var("n"))
    )
  )

  val Succ = Class("Succ", "Nat",
    fields = List(
      Field("prev", "Nat")
    ),
    methods = List(
      Method("plus", "Nat", List(Argument("n", "Nat")),
        Invoke(FieldAccess(Var("this"), "prev"), "plus", List(Invoke(Var("n"), "succ")))))
  )

  val ct = buildClassTable(List(Nat, Zero, Succ))

  ct.values.foreach(println)

  val zero = New("Zero")
  val one = New("Succ", List(zero))
  val two = New("Succ", List(one))
  val three = New("Succ", List(two))

  println(zero + ": " + exprType(zero)(Map.empty, ct))
  println(one + ": " + exprType(one)(Map.empty, ct))
  println(two + ": " + exprType(two)(Map.empty, ct))
  println(three + ": " + exprType(three)(Map.empty, ct))

  val program = Program(ct, Invoke(three, "plus", List(two)))

  println("3 + 2 = ...")
  println(evalProg(program))
}
