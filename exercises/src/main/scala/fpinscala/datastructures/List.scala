package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def appendViaFoldRight[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)((x, acc) => Cons(x, acc))

  def concat[A](a: List[List[A]]): List[A] = foldLeft(a, List[A]())(append)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a,b) => f(b, a))
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(y, ys) => ys
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(y, ys) => Cons(h, ys)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(y, ys) if f(y) => dropWhile(ys, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def loop(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(y, ys) => ys match {
        case Nil => acc
        case Cons(_, zs) => loop(ys, append(acc, List(y)))
      }
    }
    loop(l, Nil)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, sum) => 1 + sum)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a,b) => f(b, a))

  // recursive, not stack-safe
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(foldLeft2(t, z)(f), h)
  }

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length3(l: List[Int]) = foldLeft(l, 0)((sum, _) => sum + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc: List[A], h: A) => Cons(h, acc))

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def double2String(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a, b) => Cons(a.toString , b))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, b) => Cons(f(a) , b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((h,t) => append(f(h), t))

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))
}