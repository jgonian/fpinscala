package fpinscala.gettingstarted

import fpinscala.gettingstarted.MyModule._
import fpinscala.gettingstarted.PolymorphicFunctions._
import org.scalatest.WordSpec

class MyModuleTest extends WordSpec {

  "Exercise 1" should {
    "compute the nth fibonacci number" in {
      assert(fib(0) == 0)
      assert(fib(1) == 1)
      assert(fib(2) == 1)
      assert(fib(3) == 2)
      assert(fib(4) == 3)
      assert(fib(5) == 5)
      assert(fib(6) == 8)
      assert(fib(7) == 13)
      assert(fib(8) == 21)
      assert(fib(9) == 34)
      assert(fib(10) == 55)
    }
  }

  "Exercise 2" should {
    "Check whether empty Array[] is sorted" in {
      assert(isSorted(Array(), (x: Int, y: Int) => x > y))
    }

    "Check whether Array[Int] is sorted" in {
      assert(isSorted(Array(1, 2, 2, 3, 4), (x: Int, y: Int) => x <= y))
      assert(!isSorted(Array(5, 2, 3, 4), (x: Int, y: Int) => x <= y))
      assert(isSorted(Array(4, 3, 2, 1), (x: Int, y: Int) => x > y))
      assert(!isSorted(Array(4, 3, 1, 2), (x: Int, y: Int) => x > y))
    }

    "Check whether Array[String] is sorted" in {
      assert(isSorted(Array("a", "b", "c"), (x: String, y: String) => x < y))
      assert(!isSorted(Array("c", "a", "b"), (x: String, y: String) => x > y))

      assert(isSorted(Array("aaaa", "aa", "aa", "a"), (x: String, y: String) => x.size >= y.size))
    }
  }
}
