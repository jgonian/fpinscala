package fpinscala.errorhandling

import org.scalatest.FunSpec

class OptionTest extends FunSpec {

  describe("map") {
    it("should transform when value is defined") {
      assert(Some(3).map(_ + 1) == Some(4))
    }
    it("should not transform when value is undefined") {
      val empty: Option[Int] = None
      assert(empty.map(_ + 1) == None)
    }
  }

  describe("getOrElse") {
    it("should return the value if Some") {
      assert(Some(3).getOrElse(0) == 3)
    }

    it("should return super-type value if None") {
      class Fruit()
      class Apple(val name: String) extends Fruit()

      val apple: Option[Apple] = None
      val fruit: Fruit = new Fruit()

      assert(apple.getOrElse(fruit) == fruit)
    }

    it("should return default when value is undefined") {
      val empty: Option[Int] = None
      assert(empty.getOrElse("2") == "2")
    }
  }

  describe("flatMap") {
    it("should transform when value is defined") {
      assert(Some(3).flatMap(x => Some(x.toString)) == Some("3"))
    }
    it("should not transform when value is undefined") {
      val empty: Option[Int] = None
      assert(empty.flatMap(x => Some(x.toString)) == None)
    }
  }

  describe("orElse") {
    it("should return Some when value is defined") {
      assert(Some(3).orElse(Some(5)) == Some(3))
    }
    it("should return default when value is undefined") {
      val empty: Option[Int] = None
      assert(empty.orElse(Some(5)) == Some(5))
    }
  }

  describe("filter") {
    val greaterThanZero: Int => Boolean = x => x > 0

    it("should return Some when predicate is true") {
      assert(Some(3).filter(greaterThanZero) == Some(3))
    }
    it("should return None when predicate is false") {
      assert(Some(-3).filter(greaterThanZero) == None)
    }
    it("should return None when predicate value is undefined") {
      val empty: Option[Int] = None
      assert(empty.filter(greaterThanZero) == None)
    }
  }

}
