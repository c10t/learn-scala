package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
// abbreviation of "Construct"
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// companion object of List
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](ds: List[A]): List[A] = ds match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](ds: List[A], h: A): List[A] = ds match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  // Exercise 3.4
  def drop[A](ds: List[A], n: Int): List[A] =
    if (n < 1) ds
    else ds match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
  }

  // Exercise 3.5
  def dropWhile[A](ds: List[A])(f: A => Boolean): List[A] = ds match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => ds
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}