package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	def size[A](tree: Tree[A]): Int = {
		tree match {
			case Leaf(_) => 1
			case Branch(t1, t2) => size(t1) + size(t2) + 1	
		}
	}

	def maximum(tree: Tree[Int]): Int = {
		tree match {
			case Leaf(x) => x
			case Branch(t1, t2) => maximum(t1) max maximum(t2)	
		}
	}

	def depth[A](tree: Tree[A]): Int = {
		tree match {
			case Leaf(_) => 0
			case Branch(t1, t2) => 1 + (depth(t1) max depth(t2))	
		}
	}

	def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
		tree match {
			case Leaf(x) => Leaf(f(x))
			case Branch(t1, t2) => Branch(map(t1)(f),map(t2)(f))	
		}
	}

  	def fold[A,B](tree: Tree[A])(f: A=> B)(g: (B, B) => B): B = {
		tree match {
			case Leaf(x) => f(x)
			case Branch(t1, t2) => g(fold(t1)(f)(g), fold(t2)(f)(g))
		}
  	}

  	def sizeViaFold[A](tree: Tree[A]): Int = {
		fold(tree)(a => 1)(1 + _ + _)
	}

	def maximumViaFold(tree: Tree[Int]): Int = {
		fold(tree)(a => a)(_ max _)
	}

	def depthViaFold[A](tree: Tree[A]): Int = {
		fold(tree)(a => 0)((t1, t2) => 1 + (t1 max t2))
	}

	def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
		fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
	}
}