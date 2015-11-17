package fj.examples

import fj.AST._
import fj.Aux._
import fj.Types._
import fj.Eval._

object Pairs extends App {

  val A = Class("A", "Object")
  val B = Class("B", "Object")
  val Pair = Class(
    name = "Pair",
    baseClass = "Object",
    fields = List(
      Field("fst", "Object"),
      Field("snd", "Object")
    ),
    methods = List(
      Method(
        name = "setfst",
        resultType = "Pair",
        args = List(Argument("newfst", "Object")),
        body = New("Pair", List(Var("newfst"), FieldAccess(Var("this"), "snd")))
      )
    )
  )

  val ct: ClassTable = buildClassTable(List(A, B, Pair))
  val main = Invoke(New("Pair", List(New("A"), New("B"))), "setfst", List(New("B")))

  val program = Program(ct, main)

  ct.values.foreach(println)
  println(main)
  println(programType(program))
  println(evalProg(program))
}
