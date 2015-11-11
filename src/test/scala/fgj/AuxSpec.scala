package fgj

import org.specs2.mutable._
import fgj.AST._
import fgj.Types.MethodType
import fgj.Aux._

class AuxSpec extends Specification {

  "Aux.isSubclass" should {
    "treat Object in a special way" in {
      isSubclass("Object", "Object")(Map.empty) must beTrue
      isSubclass("Object", "X")(Map.empty) must beFalse
    }
    "be reflexive" in {
      isSubclass("X", "X")(Map.empty) must beTrue
    }
    "be true for directly inheriting classes" in {
      val cm = buildClassTable(List(
        Class("A", Nil, ClassType("Object")),
        Class("B", Nil, ClassType("A"))
      ))

      isSubclass("B", "A")(cm) must beTrue
      isSubclass("A", "B")(cm) must beFalse
    }
    "be transitive" in {
      val cm = buildClassTable(List(
        Class("A", Nil, ClassType("Object")),
        Class("B", Nil, ClassType("A")),
        Class("C", Nil, ClassType("B"))
      ))

      isSubclass("C", "A")(cm) must beTrue
      isSubclass("A", "C")(cm) must beFalse
    }
  }

  "Aux.substituteType" should {
    val subst = Map[TypeVarName, ClassType]("x" -> ClassType("X"))
    "replace type vars" in {
      substituteType(TypeVar("x"))(subst) must equalTo(ClassType("X"))
    }
    "remain original type vars" in {
      substituteType(TypeVar("y"))(subst) must equalTo(TypeVar("y"))
    }
    "substitute type-parametrized class types" in {
      substituteType(ClassType("Z", List(TypeVar("y"), TypeVar("x"))))(subst) must equalTo(
        ClassType("Z", List(TypeVar("y"), ClassType("X")))
      )
    }
    "substitute in type-parameterless function types" in {
      substituteType(MethodType(List(), List(TypeVar("x")), TypeVar("x")))(subst) must equalTo(
        MethodType(List(), List(ClassType("X")), ClassType("X"))
      )
    }
    "substitute in type-parametrized function types" in {
      substituteType(
        MethodType(
          List(BoundedParam(TypeVar("A"), ClassType("Object"))),
          List(TypeVar("A")),
          TypeVar("x")))(subst) must equalTo(
        MethodType(
          List(BoundedParam(TypeVar("A"), ClassType("Object"))),
          List(TypeVar("A")),
          ClassType("X")
        )
      )
    }
  }

  "Aux.fields" should {
    "return Nil for Object" in {
      fields(ClassType("Object"))(Map.empty) must equalTo(Nil)
    }
    "return fields for class definition in correct order with type replacement" in {
      val cm = Map("X" ->
        Class("X",
          List(BoundedParam(TypeVar("A"), ClassType("Object"))),
          ClassType("Object"),
          fields = List(
            Field("x", ClassType("A")),
            Field("y", TypeVar("A"))
          )
        )
      )

      fields(ClassType("X", List(ClassType("P"))))(cm) must equalTo(
        List(
          Field("x", ClassType("A")),
          Field("y", ClassType("P"))
        )
      )
    }
    "return fields for a whole hierarchy with type replacement" in {
      val cm = Map(
        "X" -> Class(
          "X",
          List(BoundedParam(TypeVar("A"), ClassType("Object"))),
          ClassType("Object"),
          fields = List(
            Field("x", ClassType("A")),
            Field("y", TypeVar("A"))
          )
        ),
        "Y" -> Class(
          "Y",
          List(BoundedParam(TypeVar("C"), ClassType("Object"))),
          ClassType("X", List(TypeVar("C"))),
          fields = List(Field("v", TypeVar("C")))
        )
      )

      fields(ClassType("Y", List(ClassType("P"))))(cm) must equalTo(List(
        Field("x", ClassType("A")),
        Field("y", ClassType("P")),
        Field("v", ClassType("P"))
      ))
    }
    "throw when trying to instantiate class with wrong number of arguments" in {
      val cm = Map("X" ->
        Class("X", List(BoundedParam(TypeVar("A"), ClassType("Object"))), ClassType("Object"))
      )
      fields(ClassType("X"))(cm) must throwAn[IllegalArgumentException]
    }
  }

  "Aux.mtype" should {
    "return None for methods in Object class" in {
      mtype("method", ClassType("Object"))(Map.empty) must beNone
    }
    "return method type with type parameters replacement" in {
      val cm = Map("X" ->
        Class(
          "X",
          List(BoundedParam(TypeVar("A"), ClassType("Object"))), ClassType("Object"),
          methods = List(Method(
            "met",
            List(BoundedParam(TypeVar("B"), ClassType("Object"))),
            TypeVar("B"),
            List(
              Argument("a", TypeVar("A")),
              Argument("b", TypeVar("B")),
              Argument("c", ClassType("C"))
            ),
            Var("a")
          ))
        )
      )

      mtype("met", ClassType("X", List(ClassType("G"))))(cm) must beSome(
        MethodType(
          List(BoundedParam(TypeVar("B"), ClassType("Object"))),
          List(ClassType("G"), TypeVar("B"), ClassType("C")),
          TypeVar("B")
        )
      )
    }
    "return method type from superclass with type parameters replacement" in {
      val cm = Map(
        "X" -> Class("X", List(BoundedParam(TypeVar("A"), ClassType("Object"))), ClassType("Object"),
          methods = List(Method(
            "met",
            List(BoundedParam(TypeVar("B"), ClassType("Object"))),
            TypeVar("B"),
            List(
              Argument("a", TypeVar("A")),
              Argument("b", TypeVar("B")),
              Argument("c", ClassType("C"))
            ),
            Var("a")))
        ),
        "Y" -> Class(
          "Y",
          List(BoundedParam(TypeVar("H"), ClassType("Object"))),
          ClassType("X", List(TypeVar("H")))
        )
      )

      mtype("met", ClassType("Y", List(ClassType("G"))))(cm) must beSome(
        MethodType(
          List(BoundedParam(TypeVar("B"), ClassType("Object"))),
          List(ClassType("G"), TypeVar("B"), ClassType("C")),
          TypeVar("B")
        )
      )
    }
  }

  "Aux.mbody" should {
    "return method expression with type replacement" in {
      val cm = Map(
        "X" -> Class(
          "X",
          List(BoundedParam(TypeVar("A"), ClassType("Object"))),
          ClassType("Object"),
          methods = List(Method(
            "m",
            List(BoundedParam(TypeVar("C"), ClassType("Object"))),
            TypeVar("A"),
            List(Argument("x", TypeVar("A"))),
            New(ClassType("B", List(TypeVar("A"), TypeVar("C")))))
          )
        ),
        "Y" -> Class(
          "Y",
          List(),
          ClassType("X", List(ClassType("Z")))
        )
      )

      mbody("m", List(ClassType("K")), ClassType("X", List(ClassType("L"))))(cm) must equalTo(
        Method(
          "m",
          List(BoundedParam(TypeVar("C"), ClassType("Object"))),
          ClassType("L"),
          List(Argument("x", ClassType("L"))),
          New(ClassType("B", List(ClassType("L"), ClassType("K"))))
        )
      )
      mbody("m", List(ClassType("K")), ClassType("Y"))(cm) must equalTo(
        Method(
          "m",
          List(BoundedParam(TypeVar("C"), ClassType("Object"))),
          ClassType("Z"),
          List(Argument("x", ClassType("Z"))),
          New(ClassType("B", List(ClassType("Z"), ClassType("K"))))
        )
      )
    }
  }

}
