package karazin.scala.users.group.week2.homework

import scala.annotation.targetName
import scala.math.{abs, signum}

object Homework:

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer = x / g
    val denom = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    @targetName("addition")
    infix def +(that: Rational): Rational =
      new Rational(
        this.numer * that.denom + that.numer * this.denom,
        this.denom * that.denom
      )

    @targetName("negation")
    infix def unary_- : Rational =
      new Rational(-this.numer, this.denom)

    @targetName("subtraction")
    infix def -(that: Rational): Rational =
      this + (-that)

    @targetName("multiplication")
    infix def *(that: Rational): Rational =
      new Rational(
        this.numer * that.numer,
        this.denom * that.denom
      )

    @targetName("division")
    infix def /(that: Rational): Rational =
      new Rational(
        this.numer * that.denom,
        this.denom * that.numer
      )

    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)

    override def equals(other: Any): Boolean =
      other match {
        case that: Rational => this.numer == that.numer && this.denom == that.denom
        case _ => false
      }

  end Rational

end Homework