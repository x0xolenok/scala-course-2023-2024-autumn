package karazin.scala.users.group.week2.homework

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import karazin.scala.users.group.week2.homework.arbitraries
import Homework._
import karazin.scala.users.group.week2.arbitraries.restricted.{Integer, NegativeInteger, PositiveInteger, Zero}
import utils._
import scala.language.implicitConversions

object HomeworkSpecification extends Properties("Homework"):

  import arbitraries.{
    given Arbitrary[Int], given Arbitrary[Rational], given Arbitrary[NegativeInteger],
    given Arbitrary[Integer], given Arbitrary[Zero], given Arbitrary[PositiveInteger]
  }

  property("throw exception due to zero denominator") = forAll { (numer: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, 0)
    }
  }

  property("throw exception due to negative denominator") = forAll { (numer: Int, kindaDenom: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, -abs(kindaDenom))
    }
  }

  property("check that rational number is simplified") = forAll { (numer: Integer, int: PositiveInteger) ⇒
    val denom = abs(int) + 1
    val rational = Rational(numer, denom)

    rational.numer == (numer / gcd(abs(numer), denom)) && rational.denom == (denom / gcd(abs(numer), denom))
  }

  property("check equals") = forAll { (left: Rational, right: Rational) ⇒
    (left == right) == (left.numer == right.numer && left.denom == right.denom)
  }

  property("less then") = forAll { (left: Rational, right: Rational) =>
    (left < right) == (left.numer * right.denom < right.numer * left.denom)
  }

  property("less or equal") = forAll { (left: Rational, right: Rational) =>
    (left <= right) == ( left < right || left == right)
  }

  property("greater") = forAll { (left: Rational, right: Rational) =>
    (left > right) == !(left <= right)
  }

  property("greater or equal") = forAll { (left: Rational, right: Rational) =>
    (left >= right) == ( left > right || left == right)
  }

  property("negation") = forAll { (rational: Rational) =>
    val negation = -rational
    val negationFunc = Rational(-rational.numer, rational.denom)
    negation == negationFunc
  }

  property("addition") = forAll { (left: Rational, right: Rational) =>
    val addition = left + right
    val additionFunc = Rational(left.numer * right.denom + right.numer * left.denom, left.denom * right.denom)
    addition == additionFunc
  }

  property("subtraction") = forAll { (left: Rational, right: Rational) =>
    val subtraction = left - right
    val subtractionFunc = Rational(left.numer * right.denom - right.numer * left.denom, left.denom * right.denom)
    subtraction == subtractionFunc
  }

  property("multiplication") = forAll { (left: Rational, right: Rational) =>
    val multiplication = left * right
    val multiplicationFunc = Rational(left.numer * right.numer, left.denom * right.denom)
    multiplication == multiplicationFunc
  }

  property("divisionPositive") = forAll { (left: Rational, numer: PositiveInteger, denom: PositiveInteger) =>
    val right = Rational(numer, denom)
    val res = left / right
    val divPositive = Rational(left.numer * right.denom, left.denom * right.numer)
    res == divPositive
  }

  property("divisionNegative") = forAll { (left: Rational, numer: NegativeInteger, denom: PositiveInteger) =>
    val right = Rational(numer, denom)
    val res = left / right
    val sign = Math.signum((left.denom * right.numer).toFloat).toInt
    val divNegative = Rational(left.numer * right.denom * sign, abs(left.denom * right.numer))
    res == divNegative
  }

  property("division by zero") = forAll { (left: Rational, zero: Zero) =>
    throws(classOf[IllegalArgumentException]) {
      val res = left / Rational(zero)
    }
  }

end HomeworkSpecification
