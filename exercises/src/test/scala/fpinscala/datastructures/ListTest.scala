package fpinscala.datastructures

import fpinscala.datastructures.List.tail
import org.scalatest.WordSpec

class ListTest extends WordSpec {

  "pattern matching expression" should {
    "be" in {
      import fpinscala.datastructures.List._
      assert(x == 3)
    }
  }

  "Exercise 3.2 - Implement tail" should {
    "return tail if list is non-empty" in {
      assert(tail(List(1,2,3)) == List(2,3))
    }
    "return Nil if list is empty" in {
      assert(tail(List()) == Nil)
    }
  }

}
