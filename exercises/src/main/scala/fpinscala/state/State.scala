package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  case class ValAndRNG[A](v: A, rng: RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val res = rng.nextInt
    //val vrng = ValAndRNG(res._1, res._2)
    //val vrng2 = (ValAndRNG.apply _) tupled res
    if (res._1 < 0) (-(res._1 + 1), res._2)
    else res
  }

  def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (num, seed) = rng.nextInt
    if (num == Int.MinValue) (0, seed)
    else (Math.abs(num), seed)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (posInt1, rng2) = nonNegativeInt(rng)
    val (posInt2, _) = nonNegativeInt(rng2)
    val dblVal =
      if (posInt1 > posInt2) posInt2 / posInt1.doubleValue()
      else posInt1 / posInt2.doubleValue()
    (dblVal, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val intVal = rng.nextInt
    val doubleVal = double(intVal._2)
    ((intVal._1, doubleVal._1), doubleVal._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val d1 = double(rng)
    val d2 = double(d1._2)
    val d3 = double(d2._2)
    ((d1._1, d2._1, d3._1), d3._2)
  }

  def doubleWithMap(rng: RNG): Rand[Double] = {
    map[Int, Double](nonNegativeInt)(x => x / Int.MaxValue + 1)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, rng: RNG, result: List[Int]): (List[Int], RNG) = {
      if (count <= 0)
        (result, rng)
      else {
        val (n, rng2) = rng.nextInt
        go(count - 1, rng2, result :+ n)
      }
    }
    go(count, rng, List.empty[Int])
  }

  def intsWithWhile(count: Int)(rng: RNG): (List[Int], RNG) = {
    var ct = count
    var res = Vector[Int]()
    var tempRNG = rng
    while (ct > 0) {
      val (n, rng2) = tempRNG.nextInt
      res = res :+ n
      tempRNG = rng2
      ct -= 1
    }
    (res.toList, rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
