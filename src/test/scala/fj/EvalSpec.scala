package fj

import org.specs2.mutable._
import AST._
import Aux._
import Eval._

class EvalSpec extends Specification {

  "Eval.eval" should {

    val cm = buildClassTable(
      List(
        Class("Nat", "Object",
          methods = List(Method("succ", "Nat", List(), New("Succ", List(Var("this")))))),
        Class("Zero", "Nat",
          methods = List(Method("plus", "Nat", List(Argument("n", "Nat")), Var("n")))
        ),
        Class("Succ", "Nat",
          fields = List(Field("prev", "Nat")),
          methods = List(
            Method("plus", "Nat", List(Argument("n", "Nat")),
              Invoke(FieldAccess(Var("this"), "prev"), "plus", List(Invoke(Var("n"), "succ")))))
        )
      )
    )
    val env = Map("x" -> Value("Zero"))

    "evaluate variable" in {
      eval(Var("x"))(env, cm) must equalTo(Value("Zero"))
    }

    "evaluate field access" in {
      val one = New("Succ", List(New("Zero")))
      eval(FieldAccess(one, "prev"))(env, cm) must equalTo(Value("Zero"))
      eval(FieldAccess(one, "nonExisting"))(env, cm) must throwA[RuntimeException]
    }

    "invoke method" in {
      val zero = New("Zero")
      val one = Invoke(zero, "succ")
      val two = Invoke(one, "succ")
      eval(Invoke(zero, "succ"))(env, cm) must equalTo(Value("Succ", List(Value("Zero"))))
      eval(Invoke(zero, "nonExisting"))(env, cm) must throwA[RuntimeException]
      val val3 = Value("Succ", List(Value("Succ", List(Value("Succ", List(Value("Zero")))))))
      eval(Invoke(one, "plus", List(two)))(env, cm) must equalTo(val3)
    }

    "construct an object" in {
      eval(New("Object"))(env, cm) must equalTo(Value("Object"))
      eval(New("Succ", List(New("Zero"))))(env, cm) must equalTo(Value("Succ", List(Value("Zero"))))
    }

    "perform a downcast" in {
      eval(Cast("Nat", New("Zero")))(env, cm) must equalTo(Value("Zero"))
      eval(Cast("Object", New("Zero")))(env, cm) must equalTo(Value("Zero"))
      eval(Cast("Object", New("Object")))(env, cm) must equalTo(Value("Object"))
      eval(Cast("Nat", New("Object")))(env, cm) must throwA[RuntimeException]
      eval(Cast("Succ", New("Zero")))(env, cm) must throwA[RuntimeException]
    }
  }

}
