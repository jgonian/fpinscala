package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.WordSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class ListTest extends WordSpec with TableDrivenPropertyChecks {

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

  "Exercise 3.10 - Implement foldLeft" should {
    "show that foldRight is not stack-safe" in {
      intercept[StackOverflowError]{
        foldRight(List(scala.collection.immutable.List.fill(10000)(1): _*), 0)(_ + _) // will throw a stack overflow error
      }
    }
    "calculate sum using foldLeft" in {
      assert(foldLeft(List(1, 2, 4), 0)(_ + _) == 7)
    }
    "calculate product using foldLeft" in {
      assert(foldLeft(List(1, 2, 4), 1)(_ * _) == 8)
      assert(foldLeft(List(1, 2, 0, 4), 1)(_ * _) == 0)
    }
    "return the specified element if list is empty" in {
      assert(foldLeft(List[Int](), 10)(_ + _) == 10)
    }
  }

  "Exercise 3.11 - Write sum, product, and a function to compute the length of a list using foldLeft." should {

    "compute the sum of a list" in {
      val data = Table(
        ("List", "Sum"),
        (List[Int](), 0),
        (List(5), 5),
        (List(5, -5), 0),
        (List(1, 2, 4), 7)
      )
      forAll(data) { (list, acc) =>
        assert(sum(list) == acc)
        assert(sum2(list) == acc)
      }
    }

    "compute the product of a list" in {
      val data = Table(
        ("List", "Res"),
        (List[Double](), 1),
        (List(1.0, 2.0, 0.0, 4.0), 0),
        (List(1.0, 2.0, 4.0), 8.0)
      )
      forAll(data) { (list, res) =>
        assert(product(list) == res)
        assert(product2(list) == res)
      }
    }
  }

  "compute the length of a list" in {
    val data = Table(
      ("List", "Length"),
      (List(), 0),
      (List(1,2,3,4,5), 5)
    )
    forAll(data) { (list, len) =>
      assert(length(list) == len)
    }
  }


}
