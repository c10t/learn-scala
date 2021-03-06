import fpinscala.datastructures._
import fpinscala.datastructures.List._

object Chapter3 extends App {
  def ex31(): Int = {
    List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 43
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
  }

  def ex39(): Int = length(List(1, 2, 3, 4))

  println("Exercise 3.9: %d".format(ex39()))
}
