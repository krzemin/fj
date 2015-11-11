package fgj

import org.specs2.mutable._
import fgj.Types._
import fgj.AST._

class TypesSpec extends Specification {

  "Types.bound" should {
    val d = Map("A" -> ClassType("A"))
    "be identity for class types" in {
      bound(ClassType("X"))(d) must equalTo(ClassType("X"))
    }
    "return bound of type variable" in {
      bound(TypeVar("A"))(d) must equalTo(ClassType("A"))
    }
  }

}
