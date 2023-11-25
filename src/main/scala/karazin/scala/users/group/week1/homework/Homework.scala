package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    val int = 42

    def not(b: Boolean): Boolean =  {
      if (b) {
        false
      } else {
        true
      }
    }
    
    def and(left: Boolean, right: Boolean): Boolean = {
      if (left) {
        right
      } else {
        false
      }
    }

    def or(left: Boolean, right: Boolean): Boolean = {
      if (left) {
        true
      } else right
    }

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (left, right) => {
      @tailrec
      def multiplicationReq(left: BigInt, right: BigInt, acc: BigInt): BigInt = {
        if (`Boolean Operators`.and(right < 0, left < 0)) {
          multiplicationReq(-left, -right, acc)
        } else if (right < 0) {
          multiplicationReq(left, right + 1, acc - left)
        } else if (`Boolean Operators`.or(right == 0, left == 0)) {
          acc
        }
        else multiplicationReq(left, right - 1, acc + left)
      }

      multiplicationReq(left, right, acc = 0)
    }


    val power: (BigInt, BigInt) => BigInt = (left, right) => {
      @tailrec
      def powerRecursive(x: BigInt, y: BigInt, res: BigInt): BigInt = {
        if (y == 0) {
          res;
        } else if (y < 0) {
          powerRecursive(x, y + 1, multiplication(x, res));
        } else {
          powerRecursive(x, y - 1, multiplication(x, res));
        }
      }

      powerRecursive(left, right, 1);
    }


    val fermatNumber: Int => BigInt = n => {
      require(n >= 0, "Negative n is not allowed")
      power(BigInt(2), power(BigInt(2), BigInt(n))) + 1
    }

  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = num => {
      def calculateNextTerm(s: String): String = {
        s.foldLeft(List.empty[(Char, Int)]) {
          case (Nil, char) =>
            (char, 1) :: Nil
          case ((prevChar, count) :: tail, char) if prevChar == char =>
            (char, count + 1) :: tail
          case (acc, char) =>
            (char, 1) :: acc
        }.reverse.map { case (digit, count) => s"$count$digit" }.mkString
      }

      @tailrec
      def generateModifiedSequence(current: String, remaining: Int): String =
        if (remaining == 1) {
          current
        } else {
          generateModifiedSequence(calculateNextTerm(current), remaining - 1)
        }

      require(num > 0, "num is more than 0")
      BigInt(generateModifiedSequence("1", num))
    }

  end `Look-and-say Sequence`
 
end Homework

