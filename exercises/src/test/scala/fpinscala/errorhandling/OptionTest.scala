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

  describe("variance") {
    it("should return the variance of non-empty seq") {
      assert(Option.variance(Seq(1.0, 2.0, 3.0, 4.0, 5.0)) == Some(2.0))
    }
    it("should return the variance of empty seq") {
      assert(Option.variance(Seq()) == None)
    }
  }

  describe("map2") {
    it("should transform if both options are defined") {
      assert(Option.map2(Some("a"), Some("b"))((a,b) => a + b) == Some("ab"))
    }
    it("should return None if any option is undefined") {
      val empty: Option[String] = None
      assert(Option.map2(empty, empty)((a,b) => a + b) == empty)
      assert(Option.map2(empty, Some("b"))((a,b) => a + b) == empty)
      assert(Option.map2(Some("a"), empty)((a,b) => a + b) == empty)
    }
  }

  describe("sequence") {
    it("should return an Option[List[A]] if sequence contains defined values") {
      assert(Option.sequence(List(Some("a"), Some("b"), Some("c"))) == Some(List("a", "b", "c")))
    }
    it("should return None if sequence contains undefined values") {
      val empty: Option[List[String]] = None
      assert(Option.sequence(List(None, Some("b"), Some("c"))) == empty)
    }
  }

  describe("traverse") {
    it("should return Some[List[A]] if sequence can be transformed") {
      assert(Option.traverse(List("1", "2", "3"))(a => Some(a.toInt)) == Some(List(1, 2, 3)))
    }
    it("should return None if sequence cannot be transformed") {
      val empty: Option[List[Int]] = None
      assert(Option.traverse(List("1", "not a number", "3"))(a => Try(a.toInt)) == empty)
    }
    it("should return Some with empty list if input sequence is empty") {
      assert(Option.traverse(List[String]())(a => Try(a.toInt)) == Some(List()))
    }
  }

  def Try[A](a: => A): Option[A] = {
    try {
      Some(a)
    } catch {
      case e: Exception => None
    }
  }

}
