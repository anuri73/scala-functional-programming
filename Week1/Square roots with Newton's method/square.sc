def sqrt(x: Double) = {

  def abs(x: Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) / x < 0.0000001

  def improve(guess: Double) = (guess + x / guess) / 2

  sqrtIter(1.0)

}

println(sqrt(10))
