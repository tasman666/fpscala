package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class FutureWithTimeout[A, B, C](f1: Future[A], f2: Future[B])(f: (A, B) => C) extends Future[C] {
    var cache: Option[C] = None

    def isDone = cache.isDefined

    def get(timeout: Long, units: TimeUnit) = cache match {
      case Some(c) => c
      case None =>
        val timeoutMillis = TimeUnit.MILLISECONDS.convert(timeout, units)
        val startTime = System.currentTimeMillis()
        val a = f1.get(timeoutMillis, TimeUnit.MILLISECONDS)
        val b = f2.get(timeoutMillis - (System.currentTimeMillis() - startTime), TimeUnit.MILLISECONDS)
        cache = Some(f(a, b))
        cache.get
    }

    def get() = cache match {
      case Some(c) => c
      case None =>
        cache = Some(f(f1.get(), f2.get()))
        cache.get
    }

    def isCancelled = f1.isCancelled || f2.isCancelled

    def cancel(evenIfRunning: Boolean): Boolean = f1.cancel(evenIfRunning) || f2.cancel(evenIfRunning)
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map2Timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      FutureWithTimeout(af, bf)(f)
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    (es: ExecutorService) => {
      val futures = ps.map(par => par(es))
      UnitFuture(futures.map(f => f.get))
    }

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence_simple(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sumPar(ints: IndexedSeq[Int]): Par[Int] = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.length <= 1) {
      Par.unit(ints.headOption getOrElse 0) // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    } else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      Par.map2Timeout(Par.fork(sumPar(l)), Par.fork(sumPar(r)))(_ + _)
    }


  def main(args: Array[String]): Unit = {
    val executor: ExecutorService = Executors.newFixedThreadPool(10)
    calculate("Sum", executor, Examples.sumPar(IndexedSeq(1, 2, 3, 4, 34, 456, 436, 456, 4)));
    calculate("Product", executor, Par.parMap(List(3, 4, 5, 6, 7, 8, 9, 10, 11,12,13,13,14,14))(a => a * 3))
    calculate("Filter", executor, Par.parFilter(List(3, 4, 5, 6, 7, 8, 9, 10, 11,12,13,13,14,14))(a => a % 2 == 0))
    executor.shutdown()
  }

  def calculate[A](label: String, executor: ExecutorService, par: Par[A]) = {
    val result = Par.run(executor)(par)
    printf("Result %s: " + result.get(4, TimeUnit.SECONDS), label)
    println()
  }
}