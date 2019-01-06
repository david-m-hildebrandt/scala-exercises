package questions

// http://aperiodic.net/phil/scala/s-99/

object PracticeQuestions {

  /*
    P01 (*) Find the last element of a list.
    Example:
      scala> last(List(1, 1, 2, 3, 5, 8))
      res0: Int = 8
  */
  def last[T](list: List[T]): T =
    list match {
      case null | Nil => throw new IllegalArgumentException("List is null or empty")
      case x :: Nil => x
      case head :: tail => last(tail)
    }


  def avg(as: List[Double]): Double =  {

    def doAverage (acc: Double, n: Int, as: List[Double]) : Double = as match {
      case Nil | null => throw new IllegalArgumentException("List is empty or null")
      case x :: Nil => (acc + x) / (n + 1)
      case x :: tail => doAverage(acc +x, n+1, tail)
    }

    doAverage(0, 0, as)
  }


  /*
    P02 (*) Find the last but one element of a list.
    Example:
      scala> penultimate(List(1, 1, 2, 3, 5, 8))
      res0: Int = 5
  */
  def penultimate[T](list: List[T]): T = list match {
    case Nil | null => throw new IllegalArgumentException("List is empty is null")
    case y :: Nil => throw new IllegalArgumentException("List contains only one element")
    case x :: y :: Nil => x
    case x :: tail => penultimate(tail)
  }

  /*
    P03 (*) Find the Kth element of a list.
    By convention, the first element in the list is element 0.
    Example:
      scala> nth(2, List(1, 1, 2, 3, 5, 8))
      res0: Int = 2
  */

  def kth[T](k: Int, list: List[T]): T = {

    def ith(i: Int, list: List[T]): T = list match {
      case x :: tail => i match {
        case 0 => x
        case _ => ith(i - 1, tail)
      }
      case Nil => throw new IllegalArgumentException("List cannot be null.")
    }

    if (list == null || list.isEmpty) throw new IllegalArgumentException("List cannot be null or empty.")
    else if (k < 0) throw new IllegalArgumentException("Index cannot be negative.")
    else if (k >= list.size) throw new IllegalArgumentException("k cannot be an index greater than list size")
    else ith(k, list)
  }


  /*  def kth(k: Int, as: List[Any]): Any = {
      def ith(i: Int, as: List[Any]): Any = as match {
        case Nil => throw new RuntimeException(s"element ${k} could not be found in list")
        case x :: tail => if (i == k) x else ith(i + 1, tail)
      }

      as match {
        case Nil | null => throw new IllegalArgumentException("List is empty or null")
        case x :: _ => if ((k >= 0) && (k < as.length)) ith(0, as) else
          throw new IllegalArgumentException("k is negative or too large for this list")
      }
    }
  */
  /*
    P04 (*) Find the number of elements of a list.
    Example:
      scala> length(List(1, 1, 2, 3, 5, 8))
      res0: Int = 6
  */

  def length[T](list: List[T]): Int = {

    def count(acc: Int, list: List[T]): Int = list match {
      case Nil => acc
      case x :: Nil => acc + 1
      case x :: tail => count(acc + 1, tail)
    }

    if (list == null) throw new IllegalArgumentException("List cannot be null.")
    else count(0, list)
  }

  /*
    P05 (*) Reverse a list.
    Example:
      scala> reverse(List(1, 1, 2, 3, 5, 8))
      res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  */
  def reverse[T](as: List[T]): List[T] = as match {
    case null => throw new IllegalArgumentException("List is null.")
    case Nil => List.empty
    case x :: Nil => List(x)
    case x :: tail => reverse(tail) :+ x
  }

  /*
    P06 (*) Find out whether a list is a palindrome.
    Example:
      scala> isPalindrome(List(1, 2, 3, 2, 1))
      res0: Boolean = true
  */
  def isPalindrome[T](list: List[T]): Boolean = {

    if (list == null) throw new IllegalArgumentException("List is null.")

    def checkElements[T](l: List[T], r: List[T]): Boolean = (l, r) match {
      case (Nil, Nil) => true
      case (List(_), Nil) | (Nil, List(_)) => false
      case (x :: xs, y :: ys) => (x == y) && checkElements(xs, ys)
    }

    val forward: List[T] = list.take(list.size / 2)
    val reverse: List[T] = list.reverse.take(list.size / 2)

    checkElements(forward, reverse)
  }


  /*
    P06 (*) Find out whether a list is a palindrome.
      Example:
      scala> isPalindrome(List(1, 2, 3, 2, 1))
    res0: Boolean = true
  */


  def isPalindromeB[T](list: List[T]): Boolean = {

    if (list == null) throw new IllegalArgumentException("List is null.")

    val forward = list.take(list.size / 2)
    val reverse = list.reverse.take(list.size / 2)

    forward.zip(reverse).filter(e => e._1 != e._2).size == 0

  }

  /*
    P07 (**) Flatten a nested list structure.
    Example:
      scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
      res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */


  def flatten(list: List[Any]): List[Any] = {
    if (list == null) throw new IllegalArgumentException("List is null.")

    list match {

      case Nil => Nil
      case (x: List[_]) :: tail => flatten(x) ++ flatten(tail)
      case x :: Nil => List(x)
      case x :: tail => x :: flatten(tail)

    }
  }

  /*
    def flatten(l: List[Any]): List[Any] = l match {

      case null => throw new IllegalArgumentException("List is null")
      case Nil => Nil
      case (x: List[Any]) :: xs => flatten(x) ::: flatten(xs)
      case x :: xs => x :: flatten(xs)
    }
  */
  /*
    P08 (**) Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
    Example:
      scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */


  def compress[T](list: List[T]): List[T] = {

    if (list == null) throw new IllegalArgumentException("List is null.")

    list match {
      case Nil => List.empty
      case x :: Nil => List(x)
      case x :: y :: Nil => if (x == y) List(x) else x :: y :: Nil
      case x :: y :: tail =>
        if (x == y) compress(y :: tail) else
          x :: compress(y :: tail)
    }
  }


  /*
    def compress(as: List[Any]): List[Any] = {
      def checkForSame(last: Any, as: List[Any]): List[Any] = as match {
        case Nil => Nil // to avoid the Scala compiler warning
        case x :: Nil => if (x == last) Nil else List(x)
        case x :: xs => if (x == last) checkForSame(last, xs) else x :: checkForSame(x, xs)
      }

      as match {
        case null => throw new IllegalArgumentException("List is null")
        case Nil => Nil
        case x :: Nil => List(x)
        case x :: xs => x :: checkForSame(x, xs)
      }
    }
  */
  /*
    P09 (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.
    Example:
      scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  */


  def pack[T](list: List[T]): List[List[T]] = {

    if (list == null) throw new IllegalArgumentException("List is null.")

    // add values of the same type to the same list until values are different
    def acc(accList: List[T], list: List[T]): List[List[T]] = accList match {
      case Nil => list match {
        case Nil => Nil
        case x :: tail => acc(List(x), tail)
      }
      case a :: tail => list match {
        case Nil => List(accList)
        case x :: tail =>
          if (a == x) acc(accList :+ x, tail)
          else accList :: acc(List(x), tail)
      }
    }

    acc(List.empty, list)
  }


  /*
    def pack(as: List[Any]): List[List[Any]] = {
      def checkForSame(last: Any, subList: List[Any], mainList: List[List[Any]], as: List[Any]): List[List[Any]] = as match {
        case Nil => Nil
        case x :: Nil => if (x == last) mainList :+ (x :: subList) else mainList :+ List(x)
        case x :: xs => if (x == last) checkForSame(last, (x :: subList), mainList, xs)
        else checkForSame(x, List(x), mainList :+ subList, xs)
      }

      as match {
        case null => throw new IllegalArgumentException("List is null")
        case Nil => Nil
        case x :: Nil => List(List(x))
        case x :: xs => checkForSame(x, List(x), List(), xs)
      }
    }
  */


  /*
    P10 (*) Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
    Example:
      scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  */

  def encode[T](as: List[T]): List[(Int, T)] = {

    if (null == as) throw new IllegalArgumentException("List is null")

    def encodePackedList[T](nls: List[List[T]]): List[(Int, T)] = nls match {
      case Nil => List.empty
      case x :: tail => (x.size, x.head) :: encodePackedList(tail)
    }

    encodePackedList(pack(as))
  }

  /*
  def encode(as: List[Any]): List[(Int, Any)] = {

    def encodePackedList(ps: List[List[Any]]): List[(Int, Any)] = ps match {

      case null => throw new IllegalArgumentException("List is null")
      case Nil => Nil
      case x :: Nil => List((x.size, x.head))
      case x :: xs => (x.size, x.head) :: encodePackedList(xs)

    }

    encodePackedList(pack(as))
  }
*/


  /*
    P11 (*) Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
    Example:
      scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  */

  def encodeModified[T](list: List[T]): List[Any] = {
    if (null == list) throw new IllegalArgumentException("List is null")

    def encodeModifiedList(pl: List[List[T]]): List[Any] = pl match {
      case Nil => List.empty
      case x :: tail =>
        (if (x.size == 1) x.head
        else (x.size, x.head)) :: encodeModifiedList(tail)
    }

    encodeModifiedList(pack(list))
  }


  /*  def encodeModified(as: List[Any]): List[Any] = {

      def encodeModifiedList(ms: List[(Int, Any)]): List[Any] = ms match {

        case null => throw new IllegalArgumentException("List is null")
        case Nil => Nil
        case x :: Nil => if (x._1 == 1) List(x._2) else List((x._1, x._2))
        case x :: xs => (if (x._1 == 1) x._2 else (x._1, x._2)) :: encodeModifiedList(xs)
      }

      encodeModifiedList(encode(as))
    }
  */
  /*
    P12 (**) Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
    Example:
      scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
      res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  */

  def decode[T](as: List[(Int, T)]): List[Any] = {
    if (null == as) throw new IllegalArgumentException("List is null")
    as match {
      case Nil => List.empty
      case (n, x) :: tail => (for (i <- 1 to n) yield x).toList ++ decode(tail)
    }
  }

  /*
def decode(as: List[(Int, Any)]): List[Any] = as match {

  case null => throw new IllegalArgumentException("List is null")
  case Nil => Nil
  case (n, t) :: Nil => (for (i <- 1 to n) yield {
    t
  }).toList
  case (n, t) :: xs => (for (i <- 1 to n) yield {
    t
  }).toList ++ decode(xs)

}
*/
  def decodeB(as: List[(Int, Any)]): List[Any] = {

    def expand(n: Int, a: Any): List[Any] = (for (i <- 1 to n) yield {
      a
    }).toList

    def decodeList(as: List[(Int, Any)]): List[Any] = as match {
      case null => throw new IllegalArgumentException("List is null")
      case Nil => Nil
      case (n, t) :: Nil => expand(n, t)
      case (n, t) :: xs => expand(n, t) ++ decode(xs)
    }

    decodeList(as)
  }

  /*
    P13 (**) Run-length encoding of a list (direct solution).
    Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
    Example:
      scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  */

  def encodeDirect[T](as: List[T]): List[(Int, T)] = {

    if (null == as) throw new IllegalArgumentException("")

    def createEncodings(t: (Int, T), as: List[T]): List[(Int, T)] = as match {
      case Nil => List(t)
      case x :: tail =>
        if (x == t._2) createEncodings((t._1 + 1, t._2), tail)
        else t :: createEncodings((1, x), tail)
    }

    if (as.isEmpty) List.empty else createEncodings((1, as.head), as.tail)
  }

  /*
def encodeDirect(as: List[Any]): List[(Int, Any)] = {

  def countValue(last: (Int, Any), as: List[Any]): List[(Int, Any)] = as match {
    case Nil => Nil
    case x :: Nil => if (x == last._2) List((last._1 + 1, last._2)) else List((1, x))
    case x :: xs => if (x == last._2) countValue((last._1 + 1, last._2), xs) else (last._1, last._2) :: countValue((1, x), xs)
  }

  as match {
    case null => throw new IllegalArgumentException("List is null")
    case Nil => Nil
    case x :: Nil => List((1, x))
    case x :: xs => countValue((1, x), xs)
  }
}
*/
  /*
    P14 (*) Duplicate the elements of a list.
    Example:
      scala> duplicate(List('a, 'b, 'c, 'c, 'd))
      res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  */

  def duplicate[T](as: List[T]): List[T] = {
    if (null == as) throw new IllegalArgumentException("List is null")
    as match {
      case Nil => Nil
      case x :: tail => x :: x :: duplicate(tail)
    }
  }

  /*
    def duplicate(as: List[Any]): List[Any] = {
      if (null == as) throw new IllegalArgumentException("List is null")
      as.flatMap(a => List(a, a))
    }
  */
  /*
    P15 (**) Duplicate the elements of a list a given number of times.
    Example:
      scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
      res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  */

  def duplicateN[T](n: Int, as: List[T]): List[T] = {
    if (null == as) throw new IllegalArgumentException("List is null")
    if (n < 1) throw new IllegalArgumentException("The value n must be 1 or greater")
    as match {
      case Nil => Nil
      case x :: tail => (for (i <- 1 to n) yield x).toList ++ duplicateN(n, tail)
    }
  }

  def duplicateN2[T](n: Int, as: List[T]): List[T] = {
    if (null == as) throw new IllegalArgumentException("List is null")
    if (n < 1) throw new IllegalArgumentException("The value n must be 1 or greater")
    as.flatMap(e => for (i <- 1 to n) yield e)
  }

  /*(


    def duplicateN(n: Int, as: List[Any]): List[Any] = {
      if (null == as) throw new IllegalArgumentException("List is null")
      if (n < 1) throw new IllegalArgumentException("The value n must be 1 or greater")
      as.flatMap(t => for (i <- 1 to n) yield {
        t
      })
    }
  */
  /*
    P16 (**) Drop every Nth element from a list.
    Example:
      scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  */

  def drop[T](n: Int, as: List[T]): List[T] = {

    if (null == as) throw new IllegalArgumentException("List is null")
    if (n < 1) throw new IllegalArgumentException("n must be 1 or greater")

    def drop(n: Int, acc: Int, as: List[T]): List[T] = as match {
      case Nil => Nil
      case x :: tail => {
        if (n == acc) drop(n, 1, tail)
        else x :: drop(n, acc + 1, tail)
      }
    }

    drop(n, 1, as)
  }

  /*
  def drop(n: Int, as: List[Any]): List[Any] = {
    if (null == as) throw new IllegalArgumentException("List is null")
    if (n < 1) throw new IllegalArgumentException("The value n must be 1 or greater")

    def drop(n: Int, i: Int, ts: List[Any]): List[Any] = ts match {
      case Nil => Nil
      case x :: Nil => if (i == n) List() else List(x)
      case x :: xs => if (i == n) drop(n, 1, xs) else x :: drop(n, i + 1, xs)
    }

    drop(n, 1, as)
  }
*/

  /*
    P17 (*) Split a list into two parts.
    The length of the first part is given. Use a Tuple for your result.
    Example:
      scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 */

  def split[T](n: Int, as: List[T]): (List[T], List[T]) = {

    if (null == as) throw new IllegalArgumentException("List is null")
    if (n < 1) throw new IllegalArgumentException("The value n must be 1 or greater")

    def split(n: Int, firstPart: List[T], as: List[T]): (List[T], List[T]) = as match {
      case Nil => (firstPart, as)
      case x :: tail =>
        if (n == firstPart.size) (firstPart, x :: tail)
        else split(n, firstPart ++ List(x), tail)
    }

    split(n, List(), as)
  }


  /*
  def split[T](n: Int, as: List[T]): (List[T], List[T]) = {
    if (null == as) throw new IllegalArgumentException("List is null")
    if (n < 1) throw new IllegalArgumentException("The value n must be 1 or greater")
    (as.take(n), as.drop(n))
  }
*/
  /*
    P18 (**) Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
    Example:
      scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: List[Symbol] = List('d, 'e, 'f, 'g)
  */

  def slice[T](s: Int, e: Int, as: List[T]): List[T] = {
    if (null == as) throw new IllegalArgumentException("List is null")
    if (s < 0) throw new IllegalArgumentException("The value s must be 0 or greater")
    if (e < 0) throw new IllegalArgumentException("The value e must be 0 or greater")
    if (s >= e) throw new IllegalArgumentException("The value s must be less than or equal to e")

    def makeSlice(s: Int, e: Int, i: Int, as: List[T]): List[T] = as match {
      case Nil => Nil
      case x :: tail =>
        if (i < s) makeSlice(s, e, i + 1, tail)
        else if (i >= s && i < e) x :: makeSlice(s, e, i + 1, tail)
        else Nil
    }

    makeSlice(s, e, 0, as)
  }

  /*
    def slice[T](s: Int, e: Int, as: List[T]): List[T] = {
      if (null == as) throw new IllegalArgumentException("List is null")
      if (s < 0) throw new IllegalArgumentException("The value s must be 0 or greater")
      if (e < 0) throw new IllegalArgumentException("The value e must be 0 or greater")
      if (s >= e) throw new IllegalArgumentException("The value s must be less than or equal to e")
      as.drop(s).take(e-s)
    }
    */
  /*

  def slice(s: Int, e: Int, as: List[Any]): List[Any] = {
    if (null == as) throw new IllegalArgumentException("List is null")
    if (s < 0) throw new IllegalArgumentException("The value s must be 0 or greater")
    if (e < 0) throw new IllegalArgumentException("The value e must be 0 or greater")
    if (s >= e) throw new IllegalArgumentException("The value s must be less than or equal to e")
    as.drop(s).take(e - s)
  }
*/

  /*
    P19 (**) Rotate a list N places to the left.
    Examples:
      scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

      scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
   */

  def rotate[T](n: Int, as: List[T]): List[T] = {
    if (null == as) throw new IllegalArgumentException("List is null")

    def rotate(r: Int, i: Int, acc: List[T], as: List[T]): List[T] = as match {
      case Nil => Nil
      case x :: tail =>
        if (i < r) rotate(r, i + 1, acc :+ x, tail)
        else if (i == r) x :: tail ++ acc
        else x :: tail
    }

    val nMod = n % as.size
    val r = if (nMod < 0) nMod + as.size else nMod
    rotate(r, 0, List(), as)
  }

  /*
    def rotate(n: Int, as: List[Any]): List[Any] = {
      if (null == as) throw new IllegalArgumentException("List is null")
      val nMod = n % as.size
      val r = if (nMod < 0) as.length + nMod else nMod
      as.drop(r) ++ as.take(r)
    }
  */
  /*
    P20 (*) Remove the Kth element from a list.
    Return the list and the removed element in a Tuple. Elements are numbered from 0.
    Example:
      scala> removeAt(1, List('a, 'b, 'c, 'd))
      res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  */
  def removeAt[T](i: Int, as: List[T]): (List[T], T) = {
    if (null == as) throw new IllegalArgumentException("List is null")
    if (i < 0 || i >= as.size) throw new IllegalArgumentException("The value i is less than 1 more than the list length")

    def removeAt(i: Int, c: Int, preprendList: List[T], as: List[T]): (List[T], T) = as match {
      case Nil => (preprendList, null.asInstanceOf[T])
      case x :: tail =>
        if (c == i) (preprendList ++ tail, x)
        else removeAt(i, c + 1, preprendList :+ x, tail)
    }

    removeAt(i, 0, List(), as)
  }

  /*
    def removeAt(i: Int, as: List[Any]): (List[Any], Any) = {
      if (null == as) throw new IllegalArgumentException("List is null")
      if (i < 0 || i >= as.size) throw new IllegalArgumentException("The value i is less than 1 more than the list length")

      val t = as.drop(i).head
      (as.take(i) ++ as.drop(i + 1), t)
    }
  */
}

