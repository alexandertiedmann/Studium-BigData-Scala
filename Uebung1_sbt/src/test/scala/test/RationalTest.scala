package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import uebung1.Rational

@RunWith(classOf[JUnitRunner])
class RationalTest extends FunSuite {
  
  test("Rational Inititalisation 1") {
    val x = new Rational(1,2)
    assert(x.value === 0.5)
  }
  
  test("Rational Inititalisation 2") {
    val x = new Rational(1,2)
    assertResult("1/2"){x.toString}
  }

  test("Test requirement (denominator!=0)"){
      intercept [IllegalArgumentException] {
        new Rational(1,0)}
  }

  test("Test add two rationals") {
    val x = new Rational(1,2)
    val y = new Rational(1,4)
    assertResult("6/8"){x.add(y).toString}
  }

  test("Test multiply a int and rational"){
    val x = new Rational(1,2)
    val y : Int = 2
    assertResult("2/4"){x.mul(y).toString}
  }

  test("Test neg two rational"){
    val x = new Rational(1,2)
    val y = new Rational(1,4)
    assertResult("2/8"){x.sub(y).toString}
  }

  test("Test neg rational"){
    val x = new Rational(1,2)
    assertResult("-1/2"){x.neg.toString}
  }

  test("reduce"){
    val x = new Rational(8,16)
    assertResult("1/2"){x.reduceRat().toString}
  }

}
