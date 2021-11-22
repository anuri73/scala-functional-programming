package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def countParenthesis(
        chars: List[Char],
        openParenthesisAmount: Int = 0
    ): Boolean = {
      if (chars.isEmpty) {
        openParenthesisAmount == 0
      } else {
        val head = chars.head
        val parenthesisAmount =
          if (head == '(') openParenthesisAmount + 1
          else if (head == ')') openParenthesisAmount - 1
          else openParenthesisAmount
        if (parenthesisAmount >= 0)
          countParenthesis(chars.tail, parenthesisAmount)
        else false
      }
    }

    countParenthesis(chars)
  }

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(m: Int, c: List[Int]): Int = {
      if (c.isEmpty) 0
      else if (m - c.head == 0) 1
      else if (m - c.head < 0) 0
      else countChange(m - c.head, c) + countChange(m, c.tail)
    }
    count(money, coins.sorted)
  }
