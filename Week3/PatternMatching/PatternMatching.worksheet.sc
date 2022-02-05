trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Number(n)   => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n)    => n.toString
  case Sum(e1, e2)  => s"${show(e1)} + ${show(e2)}"
  case Var(x)       => x
  case Prod(e1, e2) => s"${showP(e1)} * ${showP(e2)}"
}

def showP(e: Expr): String = e match {
  case e: Sum => s"(${show(e)})"
  case _      => show(e)
}

val expr = Sum(Number(1), Number(1))

println(show(expr) + " = " + eval(expr))

val expr1 = Prod(expr, Var("x"))

println(show(expr1))