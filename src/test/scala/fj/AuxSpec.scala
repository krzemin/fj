package fj

import org.specs2.mutable._
import fj.AST._
import fj.Aux._
import fj.Types._

class AuxSpec extends Specification {

  "Aux.classTableToMap" should {
    "translate class table to class map" in {
      val a = Class("A", "Object")
      val b = Class("B", "Object")
      val c = Class("C", "B")
      val ct = List(a, b, c)
      buildClassTable(ct) must equalTo(
        Map("A" -> a, "B" -> b, "C" -> c)
      )
    }
  }

  "Aux.fields" should {
    "return empty list on Object" in {
      fields("Object")(Map.empty) must equalTo(Nil)
    }
    "return fields for class definition in correct order" in {
      val cm = Map("X" -> Class("X", "Object",
        fields = List(Field("x", "A"), Field("y", "B"))
      ))

      fields("X")(cm) must equalTo(List(Field("x", "A"), Field("y", "B")))
    }
    "return fields for class and its parent in correct order" in {
      val cm = Map(
        "X" -> Class("X", "Object",
          fields = List(Field("x", "A"), Field("y", "B"))
        ),
        "Y" -> Class("Y", "X",
          fields = List(Field("z", "Z"))
        )
      )

      fields("Y")(cm) must equalTo(List(Field("x", "A"), Field("y", "B"), Field("z", "Z")))
    }
  }

  "Aux.mtype" should {
    "return None for methods in Object class" in {
      mtype("method", "Object")(Map.empty) must beNone
    }
    "return None if method is not in class" in {
      val cm = Map("X" -> Class("X", "Object"))
      mtype("nonExistingMethod", "X")(cm) must beNone
    }
    "return method type in class" in {
      val cm = Map("X" -> Class(
        "X",
        "Object",
        methods = List(
          Method("method", "C", List(Argument("a", "A"), Argument("b", "B")), Var("a"))
        )
      ))

      mtype("method", "X")(cm) must beSome(
        MethodType(List(SimpleType("A"), SimpleType("B")), SimpleType("C"))
      )
    }
    "return correct method types in subclassing" in {
      val cm = Map(
        "X" -> Class("X", "Object",
          methods = List(
            Method("m1", "C", List(Argument("a", "A"), Argument("b", "B")), Var("a"))
          )
        ),
        "Y" -> Class("Y", "X",
          methods = List(
            Method("m2", "A", List(), Var("c"))
          )
        )
      )

      mtype("m1", "Y")(cm) must beSome(
        MethodType(List(SimpleType("A"), SimpleType("B")), SimpleType("C"))
      )
      mtype("m2", "Y")(cm) must beSome(
        MethodType(List(), SimpleType("A"))
      )
    }
  }

  "Aux.mbody" should {
    "return Method from class" in {
      val cm = Map(
        "X" -> Class("X", "Object",
          methods = List(
            Method("m1", "C", List(Argument("a", "A"), Argument("b", "B")), Var("a"))
          )
        ),
        "Y" -> Class("Y", "X",
          methods = List(
            Method("m2", "A", List(), Var("c"))
          )
        )
      )

      mbody("m1", "X")(cm) must equalTo(Method("m1", "C", List(Argument("a", "A"), Argument("b", "B")), Var("a")))
      mbody("m1", "Y")(cm) must equalTo(Method("m1", "C", List(Argument("a", "A"), Argument("b", "B")), Var("a")))
      mbody("m2", "Y")(cm) must equalTo(Method("m2", "A", List(), Var("c")))
    }
  }

}
