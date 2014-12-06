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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeIntViaPatternMatching(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i,r) if (i == Int.MinValue) => (-i-1,r) 
    case (i,r) if (i >= 0) => (i,r) 
    case (i,r) => (-i,r) 
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    ( i / (Int.MaxValue.toDouble + 1),r)
  }

  def doubleViaMap(): Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (i, r) = rng.nextInt
      val (d, r2) = double(r)
      ((i, d), r2)  
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
    def ints(count: Int)(rng: RNG)(l: List[Int]): (List[Int], RNG) = 
    if (count == 0) { 
        (l, rng)
    } else {
        val (i, r) = rng.nextInt
        ints(count - 1)(r)(i :: l)
    }
    ints(count)(rng)(List())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(List[A]())) ( (r, rl) =>   map2(r, rl) ( _ :: _) )

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))
   

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n:Int): Rand[Int] = flatMap(nonNegativeInt)(
    i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) {
        unit(mod)
      } else {
        nonNegativeLessThan(n)
      }
    }
  )

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
   flatMap(s)(a => unit(f(a)))   

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => map(rb) (b => f(a,b)) )

}

import State._ 

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))  
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb map (b => f(a,b)) )
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State ( s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

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
  def unit[S, A](a: A): State[S, A] = State(s => (a,s))
  def sequence[S, A](fs: List[State[S,A]]): State[S, List[A]] = 
    fs.foldRight(unit[S, List[A]](List[A]())) ( (s, sl) =>  s.map2(sl) ( _ :: _) )

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine => {
        val machineLast = inputs.foldLeft(machine)((m,input) => input match {
            case Coin if (m.locked && m.candies > 0) => Machine(false, m.candies, m.coins + 1)  
            case Turn if (m.locked == false) => Machine(true, m.candies - 1, m.coins)  
            case _ => m
          }      
        )
      ((machineLast.coins, machineLast.candies), machineLast)
    }
  )
}
