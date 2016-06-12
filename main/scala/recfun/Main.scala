package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c == 0 || c >= r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceTracker(subList: List[Char],
                         trackingParens: List[Char]): Boolean = {
        if(subList.isEmpty && trackingParens.isEmpty) return true
        if(subList.isEmpty) return false
        val curr = subList.head
        if(curr == '('){
          return balanceTracker(subList.tail, ')' +: trackingParens)
        }
        if(curr == ')' && trackingParens.length > 0 &&
          trackingParens.head == ')'){
          return balanceTracker(subList.tail, trackingParens.tail)
        }
        if(curr == ')') return false
        return balanceTracker(subList.tail, trackingParens)
      }
      balanceTracker(chars, List())
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money < 0 || coins.length == 0) return 0
      if(money == 0) return 1
      return countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
    }
  }
