package fpinscala.datastructures

import org.scalatest.FunSpec

import fpinscala.datastructures.Tree._

class TreeTest extends FunSpec {

  describe("Tree") {

    describe("size") {

      it("should counts the number of nodes (leaves and branches) in a tree") {
        assert(size(Leaf()) == 1)
        assert(size(Branch(Leaf("a"), Leaf("b"))) == 3)
        assert(size(Branch(Leaf("a"), Leaf())) == 3)
        assert(size(Branch(Branch(Leaf(1), Leaf("2")), Branch(Leaf("3"), Branch(Leaf(4), Leaf(5))))) == 9)
      }
    }

  }
}
