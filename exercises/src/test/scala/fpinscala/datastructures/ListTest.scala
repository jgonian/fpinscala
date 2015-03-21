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

  "Exercise 3.10 & Exercise 3.13 - (write foldLeft in terms of foldRight and vice-versa) - foldLeft & foldRight" should {

    "show that foldRight is not stack-safe" in {
      intercept[StackOverflowError]{
        foldRight(List(scala.collection.immutable.List.fill(10000)(1): _*), 0)(_ + _) // will throw a stack overflow error
      }
    }

    "execute foldLeft operations on list of Ints" in {
      val sum = (a: Int, b: Int) => a + b
      val multiply = (a: Int, b: Int) => a * b

      val data = Table(
        ("List", "Start value", "Result", "Binary operator"),
        (List(1, 2, 4), 0, 7, sum),
        (List[Int](), 10, 10, sum),
        (List(1, 2, 4), 1, 8, multiply),
        (List(1, 2, 0, 4), 1, 0, multiply)
      )
      forAll(data){ (list, startValue, result, op) =>
        assert(foldLeft(list, startValue)(op) == result)
        assert(foldLeftViaFoldRight(list, startValue)(op) == result)
      }
    }

    "execute foldLeft operations on list of Strings" in {
      val concat = (a: String, b: String) => a + " " + b

      val data = Table(
        ("List", "Start value", "Result", "Binary operator"),
        (List("Hello", "World", "!"), "Bob says:", "Bob says: Hello World !", concat)
      )
      forAll(data){ (list, startValue, result, op) =>
        assert(foldLeft(list, startValue)(op) == result)
        assert(foldLeftViaFoldRight(list, startValue)(op) == result)
      }
    }

    "execute foldRight operations on list of Ints" in {
      val sum = (a: Int, b: Int) => a + b
      val multiply = (a: Int, b: Int) => a * b

      val data = Table(
        ("List", "Start value", "Result", "Binary operator"),
        (List(1, 2, 4), 0, 7, sum),
        (List[Int](), 10, 10, sum),
        (List(1, 2, 4), 1, 8, multiply),
        (List(1, 2, 0, 4), 1, 0, multiply)
      )
      forAll(data){ (list, startValue, result, op) =>
        assert(foldRight(list, startValue)(op) == result)
        assert(foldRightViaFoldLeft(list, startValue)(op) == result)
      }
    }

    "execute foldRight operations on list of Strings" in {
      val concat = (a: String, b: String) => a + " " + b

      val data = Table(
        ("List", "Start value", "Result", "Binary operator"),
        (List("Hello", "World", "!"), "Bob says:", "Hello World ! Bob says:", concat)
      )
      forAll(data){ (list, startValue, result, op) =>
        assert(foldRight(list, startValue)(op) == result)
        assert(foldRightViaFoldLeft(list, startValue)(op) == result)
      }
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
        assert(sum3(list) == acc)
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
        assert(product3(list) == res)
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
      assert(length3(list) == len)
    }
  }

  "Exercise 3.12 - Write a function that returns the reverse of a list" should {
    "should return same if empty" in {
      assert(reverse(List()) == List())
    }
    "should return same if one element" in {
      assert(reverse(List(1)) == List(1))
    }
    "should return reverse if non-empty" in {
      assert(reverse(List(1,2,3,4)) == List(4,3,2,1))
    }
  }

  "Exercise 3.14 - Implement append in terms of either foldLeft or foldRight" should {
    "append list of integers" in {
      val data = Table(
        ("1st List", "2nd List", "Result"),
        (List(), List(), List()),
        (List(), List(3, 4), List(3, 4)),
        (List(1, 2), List(), List(1, 2)),
        (List(1, 2, 3), List(4, 5), List(1, 2, 3, 4, 5))
      )
      forAll(data) { (a,b,res) =>
        assert(append(a, b) == res)
        assert(appendViaFoldRight(a, b) == res)
      }
    }
  }

  "Exercise 3.15 - Concatenate a list of lists into a single list" should {
    "Concatenate lists of Ints" in {
      assert(concat(List(List(1,2,3), List(), List(4,5))) == List(1,2,3,4,5))
    }
  }

  "Exercise 3.16 - Transform a list of integers by adding 1 to each element" should {
    "Add 1" in {
      assert(add1(List[Int]()) == List[Int]())
      assert(add1(List(1,2,3)) == List(2, 3, 4))
    }
  }

  "Exercise 3.17 - Turn each value in a List[Double] into a String" ignore {}

  "Exercise 3.18 - Write a function map that generalizes modifying each element in a list while maintaining the structure of the list" ignore {}

  "Exercise 3.19 - Write a function filter that removes elements from a list unless they satisfy a given predicate" ignore {}

  "Exercise 3.20 - Write a function flatMap" ignore {}

  "Exercise 3.21 - Use flatMap to implement filter" ignore {}

  "Exercise 3.22 - Write a function that accepts two lists and constructs a new list by adding corresponding elements" ignore {}

  "Exercise 3.23 - Write a function zipWith" ignore {}
}
