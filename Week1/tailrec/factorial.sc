import scala.annotation.tailrec

@tailrec
def factorial(n: Int, i: Int = 1): Int = {
  if (n <= 1) i else factorial(n - 1, n * i)
}

println(factorial(3))
