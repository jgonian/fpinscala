package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(v) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A, B](t: Tree[A]) = fold(t)(a => 1)((a, b) => 1 + a + b)

  def maximumViaFold(t: Tree[Int]) = fold(t)(a => a)(_ max _)

  def depthViaFold(t: Tree[Int]) = fold(t)(a => 0)((a, b) => 1 + (a max b))

  def mapViaFold[A, B](t: Tree[A])(f: A => B) = fold(t)(a => Leaf(f(a)): Tree[B])((a, b) => Branch(a, b))

}