package Utils

/**
  * Contains the function necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator:
  /**
    * Calculate the factorial of a given number
    * @param n the number to compute
    * @return n!
    */
  // DONE - Part 1 Step 1
  def factorial(n: Int): Int = 
    def loop(acc: Int, n: Int): Int =
        if n==0 then acc
        else loop(acc*n,n-1)
    loop(1, n)

  /**
    * Calculate the combination of two given numbers
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  // DONE - Part 1 Step 1
  def calculateCombination(n: Int, k: Int): Int = factorial(n)/(factorial(k) * factorial(n-k))
end ClinksCalculator
