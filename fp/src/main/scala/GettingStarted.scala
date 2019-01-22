object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n - 1, n * acc)
    go(n, 1)
  }

  // exercise 2.1
  def fibb(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int = {
      if (n <= 1) acc1 else go(n - 1, acc2, acc1 + acc2)
    }

    go(n, 0, 1)
  }

  def findFirst[A](as: Array[A], p => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n + 1), as(n))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  // exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b :B) => f(a)(b)
  
  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibonacci number", 6, fibb))
  }
}
