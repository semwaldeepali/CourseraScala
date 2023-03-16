package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if c == 0 || c == r then 1 else pascal(c - 1, r - 1) + pascal(c , r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], open: Int, close: Int ): Boolean = {
      //base case if at any point close is bigger than open
      if close > open then false
      else {
        //base case if we reach end of he list
        if chars.isEmpty then {
          if open != close then false else true
        }
        else {
          val parentheses = chars.head
          if parentheses == '(' then balanceHelper(chars.tail, open + 1, close)
          else if parentheses == ')' then balanceHelper(chars.tail, open, close + 1)
          else balanceHelper(chars.tail, open, close)
        }
      }
    }

    balanceHelper(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if money < 0 || (coins.isEmpty && money !=0) then 
      0
    else if money == 0 then 
      1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
