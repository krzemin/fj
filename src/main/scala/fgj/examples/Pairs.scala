package fgj.examples

import fgj.AST._
import fgj.Types._
import erasure.Erasure._

object Pairs extends App {

  val A = Class("A", List(), ClassType("Object"))
  val B = Class("B", List(), ClassType("Object"))
  val Pair = Class(
    name = "Pair",
    typeParams = List(
      BoundedParam(TypeVar("X"), ClassType("Object")),
      BoundedParam(TypeVar("Y"), ClassType("Object"))
    ),
    baseClass = ClassType("Object"),
    fields = List(
      Field("fst", TypeVar("X")),
      Field("snd", TypeVar("Y"))
    ),
    methods = List(
      Method(
        name = "setfst",
        typeParams = List(
          BoundedParam(TypeVar("Z"), ClassType("Object"))
        ),
        resultType = ClassType("Pair", List(TypeVar("Z"), TypeVar("Y"))),
        args = List(
          Argument("newfst", TypeVar("Z"))
        ),
        body = New(
          ClassType("Pair", List(TypeVar("Z"), TypeVar("Y"))),
          List(Var("newfst"), FieldAccess(Var("this"), "snd"))
        )
      )
    )
  )

  val ct: ClassTable = fgj.Aux.buildClassTable(List(A, B, Pair))
  val main = Invoke(
    New(ClassType("Pair", List(ClassType("A"), ClassType("B"))),
      List(New(ClassType("A")), New(ClassType("B")))),
    "setfst",
    List(ClassType("B")),
    List(New(ClassType("B")))
  )

  val program = Program(ct, main)

  ct.foreach(println)
  println(main)
  println(programType(program))

  println("Its erasure:")

  val prog1@fj.AST.Program(ect, ee) = eraseProgram(program)

  ect.foreach(println)
  println(ee)
  println(fj.Types.programType(prog1))
  println(fj.Eval.evalProg(prog1))
}
