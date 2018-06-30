package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import conc._
import org.scalameter._
import scala.math.abs

/**
 * @author hendrik

 */

@RunWith(classOf[JUnitRunner])
class PiCalculationTest extends FunSuite {

  val r=16.6855393
  val epsilon= 0.01
  def isGoodEnough(a:Double, b:Double) = 
      (abs(a-b)<epsilon)
  val tries= 10000000
    
  test("PiCalculation"){
       
      val result= PiCalculation.calculatePi(tries)
      assert(isGoodEnough(result,Math.PI))
  }
  
  test("PiCalculationDistributed"){
       
     val result= PiCalculation.calculatePiDistributed(tries,2)
     assert(isGoodEnough(result,Math.PI))
  }
  
  test("calculatePiDistributedParColl"){
       
     val result= PiCalculation.calculatePiDistributedParColl(tries,2)
     assert(isGoodEnough(result,Math.PI))
  }

}
