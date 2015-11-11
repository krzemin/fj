package fj

import org.specs2.mutable._
//import org.specs2.mock._
//import java.io.PrintStream
import fj.AST._
import fj.Aux._
import fj.Types._

class TypesSpec extends Specification /*with Mockito*/ {

  val a = Class("A", "Object")
  val b = Class("B", "Object")
  val pair = Class("Pair", "Object",
    fields = List(Field("fst", "Object"), Field("snd", "Object")),
    methods = List(Method(
      "setfst", "Pair", List(Argument("newfst", "Object")),
      New("Pair", List(Var("newfst"), FieldAccess(Var("this"), "snd")))
    )))
  val boxa = Class("BoxA", "Object",
    fields = List(Field("val", "A")),
    methods = List(Method(
      "setVal", "BoxA", List(Argument("newVal", "A")),
      New("BoxA", List(Var("newVal")))
    )))
  val ct: ClassTable = buildClassTable(List(a, b, pair, boxa))
  val e = Invoke(New("Pair", List(New("A"), New("B"))), "setfst", List(New("B")))
  val program = Program(ct, e)

  "expression typing" should {
    val g: Γ = Map.empty

    "type vars" in {
      val env: Γ = Map("x" -> SimpleType("X"))
      exprType(Var("x"))(env, Map.empty) must beSome(SimpleType("X"))
    }
    "type fields" in {
      val e = New("Pair", List(New("A"), New("B")))
      exprType(FieldAccess(e, "fst"))(g, ct) must beSome(SimpleType("Object"))
      exprType(FieldAccess(e, "snd"))(g, ct) must beSome(SimpleType("Object"))
    }
    "type invoke" in {
      val e = New("Pair", List(New("A"), New("B")))
      exprType(Invoke(e, "setfst", List(New("A"))))(g, ct) must beSome(SimpleType("Pair"))
    }
    "type new" in {
      exprType(New("A"))(g, ct) must beSome(SimpleType("A"))
      exprType(New("B"))(g, ct) must beSome(SimpleType("B"))
      exprType(New("Pair", List(New("A"), New("B"))))(g, ct) must beSome(SimpleType("Pair"))
    }
    "not type bad typed new" in {
      exprType(New("BoxA" , List(New("B"))))(g, ct) must beNone
    }
    "type casts" in {
      val c = Class("C", "B")
      val ct = Aux.buildClassTable(List(a, b, c))
      exprType(Cast("Object", New("B")))(Map.empty, ct) must beSome(SimpleType("Object"))
      exprType(Cast("C", New("B")))(Map.empty, ct) must beSome(SimpleType("C"))
      exprType(Cast("B", New("B")))(Map.empty, ct) must beSome(SimpleType("B"))
//      val ps = mock[PrintStream]
//      Console.withOut(ps) {
//        exprType(Cast("A", New("B")))(emptyEnv, ct) must beSome(SimpleType("A"))
//      }
//      there was one(ps).println(s"stupid warning: (A) (${New("B")} : B)".asInstanceOf[Any])
    }
  }

  "multiple expression typing" should {
    val g: Γ = Map.empty

    "fold expressions into option of product type" in {
      val es = List(New("A"), New("B"), New("Pair", List(New("A"), New("B"))))
      exprType(es)(g, ct) must beSome(List(SimpleType("A"), SimpleType("B"), SimpleType("Pair")))
    }

    "return none if one of expressions fails type checking" in {
      val es = List(New("A"), New("B"), New("BoxA", List(New("B"))))
      exprType(es)(g, ct) must beNone
    }
  }

  "method typing" should {
    "type methods" in {
      methodTypes("Pair", "setfst")(ct) must beTrue
    }
  }

  "class typing" should {
    "type classes" in {
      classTypes("Pair")(ct) must beTrue
    }
  }

  "program typing" should {
    "type program" in {
      programType(program) must beSome(SimpleType("Pair"))
    }
  }

}
