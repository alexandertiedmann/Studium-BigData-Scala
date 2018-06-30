package conc
import java.util.concurrent._
import common._
import scala.util.DynamicVariable

object PiCalculation {
    
  /* 
   * 
   * Implementation of a distributed Pi-Calculation Method
   * using different styles.
   * 
   */
  val x = new scala.util.Random
  
  def calculateHits(tries:Int, hits:Int):Int={
    if (tries==0) hits      
      else{ 
        val x_coordinate= x.nextDouble
        val y_coordinate= x.nextDouble
        if (math.sqrt((x_coordinate*x_coordinate) + (y_coordinate*y_coordinate)) >1) 
          calculateHits(tries-1, hits)
           else calculateHits(tries-1, hits+1)
    }
  }
    
  def calculatePi(tries:Int):Double={    
  
   (4*(calculateHits(tries,0)/tries.toDouble))
  }
  
  /* 
   * Implement the calculatePi-function in a distributed manner
   * 
   * Use different processes for calculating a portion of all tries.
   * 
   * You can use the calculateHits function 
   */
  def calculatePiDistributed(tries:Int, depth:Int):Double= ??? 
  
  /* 
   * Implement the calculatePi-function in a distributed manner
   * 
   * Use parallelized collections for calculating the portion.
   * 
   * You can use the calculateHits function.
   */
  
  def calculatePiDistributedParColl(tries:Int,depth:Int):Double= ???
}