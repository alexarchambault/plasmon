// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/parsing/BinarySearch.scala or an earlier version of that file

package plasmon.ide

import scala.annotation.tailrec

private object BinarySearch {
  sealed trait ComparisonResult
  case object Greater extends ComparisonResult
  case object Equal   extends ComparisonResult
  case object Smaller extends ComparisonResult

  /** Binary search using a custom compare function.
    *
    * scala.util.Searching does not support the ability to search an IndexedSeq by a custom mapping
    * function, you must search by an element of the same type as the elements of the Seq.
    *
    * @param array
    *   Must be sorted according to compare function so that for all i > j, compare(array(i),
    *   array(i)) == Greater.
    * @param compare
    *   Callback used at every guess index to determine whether the search element has been found,
    *   or whether to search above or below the guess index.
    * @return
    *   The first element where compare(element) == Equal. There is no guarantee which element is
    *   chosen if many elements return Equal.
    */
  def array[T](
    array: Array[T],
    compare: (T, Int) => ComparisonResult
  ): Option[T] = {
    @tailrec def loop(lo: Int, hi: Int): Option[T] =
      if (lo > hi) None
      else {
        val guess = lo + (hi - lo) / 2
        val curr  = array(guess)
        compare(curr, guess) match {
          case Greater => loop(lo, guess - 1)
          case Equal   => Some(curr)
          case Smaller => loop(guess + 1, hi)
        }
      }
    loop(0, array.length - 1)
  }
}
