package fpinscala.datastructures

import fpinscala.datastructures.List._
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

  "Exercise 3.3 - Implement setHead" should {
    "set head of a non-list" in {
      assert(setHead(List(1,2,4), 5) == List(5,2,4))
    }
    "return Nil if list is empty" in {
      assert(setHead(List(), 5) == Nil)
    }
  }

  "Exercise 3.4 - Implement drop" should {
    "drop elements" in {
      assert(drop(List(1,2,3,4), 2) == List(3,4))
    }
    "return same list if we drop 0" in {
      assert(drop(List(1,2,3,4), 0) == List(1,2,3,4))
    }
    "return same list if we drop negative number" in {
      assert(drop(List(1,2,3,4), -1) == List(1,2,3,4))
    }
    "return Nil if we drop all the elements" in {
      assert(drop(List(1,2,3,4), 10) == Nil)
    }
    "return Nil if there is nothing to drop" in {
      assert(drop(List(), 2) == Nil)
    }
  }

  "Exercise 3.5 - Implement dropWhile" should {
    "drop elements while predicate matches" in {
      assert(dropWhile(List(1,2,4,5,6), (x: Int) => x < 3) == List(4,5,6))
    }
    "drop zero elements if predicate does not match" in {
      assert(dropWhile(List(1,2,4,5,6), (x: Int) => x == 10) == List(1,2,4,5,6))
    }
    "drop all predicate matches everything" in {
      assert(dropWhile(List(1,2,4,5,6), (x: Int) => x < 10) == List())
    }
  }

  "Exercise 3.6 - Implement init" should {
    "leave out last element" in {
      assert(init(List(1,2,3,4)) == List(1,2,3))
    }
    "return Nil if there is nothing to leave out" in {
      assert(init(List()) == Nil)
    }
    "return Nil if there is on element to leave out" in {
      assert(init(List(1)) == Nil)
    }
  }

  "Exercise 3.9 - Compute the length of a list using foldRight" should {
    "compute length of non empty list" in {
      assert(length(List("a", "b")) == 2)
    }
    "compute length of empty list" in {
      assert(length(List()) == 0)
    }
  }


}
