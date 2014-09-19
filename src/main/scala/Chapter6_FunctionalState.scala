package com.goral09.fp_in_scala.chapter6.PurelyFunctionalState

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ( (seed2 >>> 16).asInstanceOf[Int], simple(seed2) )
    }
  }

  def positiveInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, rng)  => (Int.MaxValue, rng)
    case (randInt, rng)       => (math.abs(randInt), rng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (d, rng2) = RNG.positiveInt(rng)
    (d / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = RNG.double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d), rng2) = RNG.intDouble(rng)
    ((d, i), rng2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = RNG.double(rng)
    val (d2, rng3) = RNG.double(rng2)
    val (d3, rng4) = RNG.double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(num: Int)(rng: RNG): (List[Int], RNG) = {
    def helper(tmp: List[Int], counter: Int)(rng2: RNG): (List[Int], RNG) = counter match {
      case 0 => (tmp, rng2)
      case _ => {
        val (i, rng3) = rng2.nextInt
        helper(i :: tmp, counter - 1)(rng3)
      }
    }
    helper(List[Int](), num)(rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng1) = s(rng)
    (f(a), rng1)
  }

  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ % n)

  def double_2: Rand[Double] = map(positiveInt)(_ /(Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rnd => {
    val (i1, r1) = ra(rnd)
    val (i2, r2) = rb(r1)
    (f(i1,i2), r2)
  }

  def intDouble_1: Rand[(Int, Double)] = map2(positiveInt, double_2)((_,_))
  def doubleInt_1: Rand[(Double, Int)] = map2(double_2, positiveInt)((_,_))
}