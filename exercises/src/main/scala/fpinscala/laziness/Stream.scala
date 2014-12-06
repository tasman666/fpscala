package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] =  
    if (n > 0) this match {
      case Cons(h, t) if n == 1 => cons(h(), Stream.empty) // we can say Stream.empty
      case Cons(h, t) => cons(h(), t().take(n-1))
      case _ => Stream.empty
    }
    else Stream()            // or Stream()

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = 
    if (n > 0) this match {
      case Cons(h, t) => t().drop(n-1)
      case _ => Stream.empty
    }
    else this  

  def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Stream.empty   
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

   def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = 
    foldRight(Stream[A]())((a, b) => if (p(a)) cons(a, b) else Stream.empty)
  
    def headOption: Option[A] = foldRight(None:Option[A])((a, b) => Some(a))

   def map[B](f: A => B): Stream[B] = 
    foldRight(empty[B])( (a, b) => cons(f(a), b))  

   def filter(f:A => Boolean): Stream[A] =
    foldRight(empty[A])( (a, b) => if (f(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)( (a, b) => cons(a,b))  

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])( (a, b) => f(a).append(b)) 

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h,t) => h() :: t().toList 
  }

  def map2[B](f: A => B): Stream[B] = unfold(this){
      case Cons(h,t) => Some( ( f(h()), t()) )
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this,n)){
      case (Cons(h,t),n) if n > 0 => Some( (h(), (t(), n - 1)) )  
      case _ => None
  } 

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
      case Cons(h,t) if p(h()) => Some( (h(), t()) )  
      case _ => None
  } 

  def zipWith[B,C](s: Stream[B])(f:(A,B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1),Cons(h2,t2)) => Some ( ( f(h1(),h2()), (t1(), t2()) )  )
      case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this,s2)) {
      case (Empty, Empty) => None  
      case (Empty,Cons(h2,t2)) => Some ( ( (None, Some(h2())), (Empty, t2()) )  )
      case (Cons(h1, t1),Empty) => Some ( ( (Some(h1()), None), (t1(), Empty))  )
      case (Cons(h1, t1),Cons(h2,t2)) => Some ( ( (Some(h1()), Some(h2())), (t1(), t2()) )  )
  }

  def startsWith[A](s: Stream[A]): Boolean = 
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] = unfold(this) {
      case Empty => None  
      case s => Some ( (s, s drop 1) )
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s) 

  def scanRightNotEfficient[B](z: B)(f: (A, => B) => B): Stream[B] =  unfold(this) {
      case Empty => None  
      case s => Some ( (s.foldRight(z)(f), s drop 1) )
    } append (Stream(z))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =  foldRight((z, Stream(z)))((a,p) => {
      val b2 = f(a,p._1)
      (b2, cons(b2,p._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail) 
    tail
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))  
  }

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = 
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case _ => Empty
  }

  val fibs2 = {
    unfold((0,1))(s => Some((s._1,(s._2, s._1 + s._2))))
  }

  val fibs2a = {
    unfold((0,1)){ case(s1, s2) => Some((s1,(s2, s1 + s2)))}
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(s => Some((s, s + 1)))  
  }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a))) 

  val onesViaUnfold: Stream[Int] = constantViaUnfold(1)  

}