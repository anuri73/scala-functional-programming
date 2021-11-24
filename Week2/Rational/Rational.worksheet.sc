class Rational(x: Int, y: Int) :

  require(y != 0, "Denominator must not be equal 0")
  require(y > 0, "Denominator must ne positive")

  def this(x:Int) = this(x, 1)

  private def gcd(a:Int, b:Int):Int = 
    if b == 0 then a else gcd(b, a % b)

  def numer = x
  def denom = y
  
  def add(r: Rational):Rational =
    Rational(
      numer * r.denom + r.numer * denom,
      denom * r.denom
    )

  def mul(r: Rational):Rational =
    Rational(
      numer * r.numer,
      denom * r.denom
    )

  def neg:Rational = Rational(
    -numer, 
    denom
  )

  def sub(r:Rational):Rational = add(r.neg)

  def less(that:Rational):Boolean = 
    numer * that.denom < that.numer * denom

  def max(that:Rational):Rational = 
    if this.less(that) then that else this

  override def toString = s"${numer / gcd(x.abs, y)}/${denom / gcd(x.abs, y)}"

end Rational

val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)
val q = Rational(3)

x.add(y).mul(z)

y.neg

x.sub(y).sub(z)