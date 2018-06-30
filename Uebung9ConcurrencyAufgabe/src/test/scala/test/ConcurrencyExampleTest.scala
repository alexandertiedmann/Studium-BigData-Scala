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
class ConcurrencyExampleTest extends FunSuite {

  val r=16.6855393
  val epsilon= 0.0000001
  def isGoodEnough(a:Double, b:Double) = 
      (abs(a-b)<epsilon)
  
  trait testArrays{
    val a= new Array[Double](1000)
    for (i <- 0 to 999) a(i)= 1+i.toDouble
    val a_par= a.clone.par
  }
  
  test("vectorCalculationImparative"){
    
    new testArrays{
      
      val result= ConcurrencyExample.vectorCalculationImparative(a)
      assert(isGoodEnough(result,r))
    }
  }
  
  test("vectorCalculationRecursive"){
    
    new testArrays{
      
      val result= ConcurrencyExample.vectorCalculationRecursive(a)
      assert(isGoodEnough(result,r))
    }
  }

  test("vectorCalculationTailRecursive"){
    
    new testArrays{
      
      val result= ConcurrencyExample.vectorCalculationTailRecursive(a)
      assert(isGoodEnough(result,r))
    }
  }
  
  test("vectorCalculationTailRecursive2"){
    
    new testArrays{
      
      val result= ConcurrencyExample.vectorCalculationTailRecursive(a)
      assert(isGoodEnough(result,r))
    }
  }
  
  test("vectorCalculationUsingFold"){
    
    new testArrays{
      
      val result= ConcurrencyExample.vectorCalculationUsingFold(a)
      assert(isGoodEnough(result,r))
    }
  }
   
  test("vectorCalculationUsingParFold"){
    
    new testArrays{
      
      val result= ConcurrencyExample.vectorCalculationUsingParFold(a_par)
      assert(isGoodEnough(result,r))
    }
  }

    test("vectorCalculationUsingParMapAndReduce"){
    
    new testArrays{
      
      val result= ConcurrencyExample.vectorCalculationUsingParMapAndReduce(a_par)
      assert(isGoodEnough(result,r))
    }
  }

  test("vectorCalculationUsingRecursiveTask"){
    
    new testArrays{
      
      val result= ConcurrencyExample.vectorCalculationUsingRecursiveTask(a)
      assert(isGoodEnough(result,r))
    }
  }
}
