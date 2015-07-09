package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, y) => y
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, tail) => Cons(h, tail)

  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n > 0) {
      l match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
    } else {
      l
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_: A, len: Int) => len + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sumFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def productFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc: List[A], elem: A) => Cons(elem, acc))


  def addOne(l: List[Int]): List[Int] = {
    map(l)(_ + 1)
  }

  def doubleToString(l: List[Double]): List[String] = map(l)(_.toString)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((elem, acc) => Cons(f(elem), acc))

  def filter[A](l: List[A])(predicate: A => Boolean): List[A] = flatMap(l)((elem) => if (predicate(elem)) Cons(elem, Nil) else Nil)

  def filterOdd(l: List[Int]): List[Int] = filter[Int](l)(_ % 2 == 0)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil: List[B])((elem, acc) => append(f(elem), acc))

  def addPairWise(first: List[Int], second: List[Int]): List[Int] = zipWith(first, second)(_ + _)

  def zipWith[A](first: List[A], second: List[A])(f: (A, A) => A): List[A] = (first, second) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, Nil) => true
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(supHead, supTail), Cons(subHead, subTail)) if supHead == subHead => hasSubsequence(supTail, subTail)
    case (Cons(supHead, supTail), Cons(subHead, subTail)) => hasSubsequence(supTail, sub)
  }
}

object Run {

  def main(args: Array[String]): Unit = {
    println(List.hasSubsequence(List(1, 2, 3), List(4, 5, 6)))
    println(List.hasSubsequence(List(3, 4, 5), List(3, 4)))
    println(List.hasSubsequence(List(3, 4, 5), List(4, 5)))
    println(List.hasSubsequence(List(3, 4, 5), List(4)))
    println(List.hasSubsequence(List(3, 4, 5), List(4, 6)))
    println(List.hasSubsequence(List(), List()))
    println(List.hasSubsequence(List(1), List()))
    println(List.hasSubsequence(List(), List(1)))
  }
}
