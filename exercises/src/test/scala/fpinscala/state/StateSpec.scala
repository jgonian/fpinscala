package fpinscala.state

import fpinscala.state.RNG.{FixedValue, Rand, Simple}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  behavior of "State"

  val rngGen: Gen[RNG] = for (seed <- arbitrary[Long]) yield Simple(seed)

  def fixedValueGen(value: Int): Gen[RNG] = Gen.const(FixedValue(value))

  val zeroValueGen: Gen[RNG] = fixedValueGen(0)
  val minIntValueGen: Gen[RNG] = fixedValueGen(Int.MaxValue)
  val maxIntValueGen: Gen[RNG] = fixedValueGen(Int.MinValue)

  implicit val arbitraryRNG = Arbitrary(Gen.frequency(
    (1, zeroValueGen),
    (1, minIntValueGen),
    (1, maxIntValueGen),
    (10, rngGen)
  ))

  it should "6.1 - generate non negative integers" in {
    forAll("rng") { (rng: RNG) =>
      val (generatedValue, newRng) = RNG.nonNegativeInt(rng)
      newRng should not be null
      generatedValue should be >= 0
    }
  }

  it should "6.2 - generate double between 0 and 1, not including 1" in {
    forAll("rng") { (rng: RNG) =>
      val (generatedValue, newRng) = RNG.double(rng)
      newRng should not be null
      generatedValue should (be >= 0.0 and be < 1.0)
    }
  }

  it should "6.3 - generate an (Int, Double) pair" in {
    forAll("rng") { (rng: RNG) =>
      val ((intValue1, doubleValue1), rng1) = RNG.intDouble(rng)
      val ((intValue2, doubleValue2), rng2) = RNG.intDouble(rng)
      intValue1 should be(intValue2)
      doubleValue1 should be(doubleValue2)
      rng1 should be(rng2)
    }
  }

  it should "6.3 - generate an (Double, Int) pair" in {
    forAll("rng", minSuccessful(1)) { (rng: RNG) =>
      RNG.doubleInt(rng)
    }
  }

  it should "6.3 - generate an (Double, Double, Double) pair" in {
    forAll("rng", minSuccessful(1)) { (rng: RNG) =>
      RNG.double3(rng)
    }
  }

  it should "6.4 - generate list of random integers" in {
    forAll("rng", minSuccessful(1)) { (rng: RNG) =>
      RNG.ints(10)(rng)
    }
  }

  it should "6.5 - generate double between 0 and 1, not including 1, using map" in {
    forAll("rng") { (rng: RNG) =>
      val (generatedValue, newRng) = RNG.double2(rng)(rng)
      newRng should not be null
      generatedValue should (be >= 0.0 and be < 1.0)
    }
  }

  it should "6.6 - implement map2" in {
    val integerRand: Rand[Int] = RNG.int
    val doubleRand: Rand[Double] = RNG.double

    val intDoubleCombined: Rand[(Int, Double)] = RNG.both(integerRand, doubleRand)

    forAll("rng", minSuccessful(1)) { (rng: RNG) =>
      println(intDoubleCombined(rng))
    }
  }

  it should "6.8 - implement flatmap and use it in nonNegativeLessThan" in {
    forAll("n", minSuccessful(1000)) { (n: Int) =>
      whenever(n > 0) {
        forAll((rngGen, "rng"), minSuccessful(1)) { (rng: RNG) =>
          val (value, r) = RNG.nonNegativeLessThan(n)(rng)
          value should (be >= 0 and be < n)
        }
      }
    }
  }

  behavior of "Candy dispenser"

  it should "foo" in {
    val initState = Machine(locked = true, candies = 5, coins = 10)
    val finalState = Machine(locked = true, candies = 1, coins = 14)

  }
}
