package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class FixedValue(constantValue: Int) extends RNG {
    override def nextInt: (Int, RNG) = (constantValue, this)
  }

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r) = rng.nextInt
    (if (v < 0) -(v + 1) else v, r)
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt) { i => i - i % 2}
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    assert(n > 0, "n must be greater than zero")
    flatMap(nonNegativeInt) { (i: Int) =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), r)
  }

  def double2(rng: RNG): Rand[Double] = {
    map(nonNegativeInt) { i => i / (Int.MaxValue.toDouble + 1) }
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (v1, rg1) = rng.nextInt
    val (v2, rg2) = double(rg1)
    ((v1, v2), rg2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, gen: RNG, res: List[Int]): (List[Int], RNG) = {
      if (count == 0) (res, gen)
      else {
        val (intValue, newGen) = gen.nextInt
        loop(count - 1, newGen, intValue :: res)
      }
    }
    loop(count, rng, List.empty)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1): (A, RNG) = ra(rng)
      val (b, r2): (B, RNG) = rb(r1)
      (f(a, b), r2)
    }
  }

  def both[A, B](a: Rand[A], b: Rand[B]): Rand[(A, B)] = map2(a, b)((a,b) => (a, b))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1): (A, RNG) = f(rng)
      g(a)(r1)
    }
  }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => _map(rb)(b => f(a, b)))
  }

}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    st => {
      val (a, s) = run(st)
      f(a).run(s)
    }
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def unit[S, A](a: A): State[S, A] = {
    State(s => (a, s))
  }

  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    // verbose
    l.reverse.foldLeft(unit[S, List[A]](List.empty[A]))((acc: State[S, List[A]], s: State[S, A]) => s.map2(acc)((a:A, b:List[A]) => a :: b))

    // same, more concise
    //l.reverse.foldLeft(unit[S, List[A]](List()))((acc, s) => s.map2(acc)(_ :: _))
  }
}
