/*
 * Write a function
 *   def solution(a: Array[Int]): Int
 *
 * that, given an array A of N integers, returns the smallest positive integer
 * (greater than 0) that does not occur in A.
 *
 * For example, given A = [1,3,6,4,1,2] the function should return 5.
 *              given A = [1,2,3], the function should return 4.
 *              given A = [-1, -3], the function should return 1
 *
 * Write an efificent algorithm for the following assumptions :
 *    N is an integer within the range [1 ... 100'000];
 *    each element of array A is
 *         an integer within the range  [-1'000'000 ... 1'000'000]
 */

// The naive solution
def solution(a: Array[Int]): Int =
  val set = a.toSet
  val max = set.max
  val rng = (1 to max)
  val missingInRange =
    rng.foldLeft[List[Int]](Nil) { (acc, n) =>
      if !set.contains(n) && n > 0
        then n :: acc
        else acc
    }

  if missingInRange.nonEmpty
    then missingInRange.min
    else 1


