package fpinscala.laziness

import org.scalatest.FlatSpec

class StreamTest extends FlatSpec {

  behavior of "Exercise 5.1"

  it should "convert a Stream to a List" in {
    assert(Stream(1,2,3).toListRecursive == List(1, 2, 3))
    assert(Stream(1,2,3).toListTailRecursive == List(1, 2, 3))
    assert(Stream(1,2,3).toList == List(1, 2, 3))
  }

  it should "convert an empty Stream to an empty List" in {
    assert(Stream().toListRecursive == List())
    assert(Stream().toListTailRecursive == List())
    assert(Stream().toList == List())
  }

}
