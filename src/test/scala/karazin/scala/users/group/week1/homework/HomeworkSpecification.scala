package karazin.scala.users.group.week1.homework

import org.scalacheck.*
import Prop.{forAll, propBoolean, throws}
import Homework.*
import karazin.scala.users.group.week1.homework.arbitraries

import scala.annotation.tailrec

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndAaSequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == !b
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    and(left, right) == (left && right)
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    or(left, right) == (left || right)
  }

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }
  }
  property("throw exception due to negative n") = forAll { (n: Int) =>
    throws(classOf[IllegalArgumentException]) {
      if(n == 0) {
        fermatNumber(-4)
      } else {
        val x = -Math.abs(n)
        fermatNumber(x)
      }
    }
  }

  property("fermatNumber") = forAll { (n: Int) =>
    fermatNumber(n) == Math.pow(2, Math.pow(2, n)) + 1
  }  

end FermatNumbersSpecification

object LookAndAaSequenceSpecification extends Properties("Look-and-say Sequence"):
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]


  property("throw exception due to negative n") = forAll { (n: Int) =>
    throws(classOf[IllegalArgumentException]) {
      if (n == 0) {
        lookAndSaySequenceElement(-3)
      } else {
        val c = -Math.abs(n)
        lookAndSaySequenceElement(c)
      }
    }
  }

  @tailrec
  private def loop(n: Int, num: String): String = {
    if (n <= 0) num else loop(n - 1, lookandsay(num))
  }

  private def lookandsay(number: String): String = {
    val result = new StringBuilder

    @tailrec
    def loop(numberString: String, repeat: Char, times: Int): String =
      if (numberString.isEmpty) result.toString()
      else if (numberString.head != repeat) {
        result.append(times).append(repeat)
        loop(numberString.tail, numberString.head, 1)
      } else loop(numberString.tail, numberString.head, times + 1)

    loop(number.tail + " ", number.head, 1)
  }

  property("lookAndSaySequence") = forAll { (n: Int) =>
    if(n == 0) {
      
      lookAndSaySequenceElement(1).toString() == loop(0, "1")
    } else {
      lookAndSaySequenceElement(n).toString() == loop(n - 1, "1")

    }
  }  

end LookAndAaSequenceSpecification
