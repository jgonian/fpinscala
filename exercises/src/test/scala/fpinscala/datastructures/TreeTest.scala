package fpinscala.datastructures

import org.scalatest.FunSpec

import fpinscala.datastructures.Tree._

class TreeTest extends FunSpec {

  describe("Exercise 3.25 - size") {
    it("should calculate the number of nodes (leaves and branches) in a tree") {
      assert(size(Leaf()) == 1)
      assert(size(Branch(Leaf("a"), Leaf("b"))) == 3)
      assert(size(Branch(Leaf("a"), Leaf())) == 3)
      assert(size(Branch(Branch(Leaf(1), Leaf("2")), Branch(Leaf("3"), Branch(Leaf(4), Leaf(5))))) == 9)

      assert(sizeViaFold(Leaf()) == 1)
      assert(sizeViaFold(Branch(Leaf("a"), Leaf("b"))) == 3)
      assert(sizeViaFold(Branch(Leaf("a"), Leaf())) == 3)
      assert(sizeViaFold(Branch(Branch(Leaf(1), Leaf("2")), Branch(Leaf("3"), Branch(Leaf(4), Leaf(5))))) == 9)
    }
  }

  describe("Exercise 3.26 - maximum") {
    it ("should calculate the maximum of an empty tree") {
      assert(maximum(Leaf(3)) == 3)
      assert(maximumViaFold(Leaf(3)) == 3)
    }
    it ("should calculate the maximum of a non-empty tree") {
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
      assert(maximum(tree) == 5)
      assert(maximumViaFold(tree) == 5)
    }
  }

  describe("Exercise 3.27 - depth") {
    it ("should calculate the depth of an empty tree") {
      assert(depth(Leaf(3)) == 0)
      assert(depthViaFold(Leaf(3)) == 0)
    }
    it ("should calculate the depth of a non-empty balanced tree") {
      assert(depth(Branch(Leaf(1), Leaf(2))) == 1)
      assert(depthViaFold(Branch(Leaf(1), Leaf(2))) == 1)
    }
    it ("should calculate the depth of a non-empty non-balanced tree") {
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
      assert(depth(tree) == 3)
      assert(depthViaFold(tree) == 3)
    }
  }

  describe("Exercise 3.28 - map") {
    it("should transform each value") {
      assert(map(Branch(Leaf("a"), Leaf("b")))(_.toUpperCase) == Branch(Leaf("A"), Leaf("B")))
      assert(mapViaFold(Branch(Leaf("a"), Leaf("b")))(_.toUpperCase) == Branch(Leaf("A"), Leaf("B")))
    }
  }

}
