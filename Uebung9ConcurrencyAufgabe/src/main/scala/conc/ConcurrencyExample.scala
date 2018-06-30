package conc
import java.util.concurrent._
import common._

object ConcurrencyExample {
    
  /* 
   * 
   * Implementation of one method with different styles:
   * 
   * Method gets an array and computes for all elements 
   * the following function:
   * 
   * f(x) = math.pow(math.sin(math.log(l(i))),2)
   * 
   * Afterwards it sums up all results
   * 
   */
  
  def vectorCalculationImparative(l:Array[Double]):Double= {
    
    var result:Double= 0.0
    for (i <- 0 to l.length-1) result = result + math.pow(math.sin(math.log(l(i))),2) 
    Math.sqrt(result)
  }
  
  def vectorCalculationRecursive(l:Array[Double]):Double= {
  
    def helper(l:Array[Double]):Double=
     l.isEmpty match {
       
       case true => 0.0
       case false => math.pow(math.sin(math.log(l.head)),2)+helper(l.tail)
     }  
    Math.sqrt(helper(l))
  }
  
  def vectorCalculationTailRecursive(l:Array[Double]):Double= {

    def helper(l:Array[Double],acc:Double):Double=
     l.isEmpty match {
       
       case true => acc
       case false => helper(l.tail,math.pow(math.sin(math.log(l.head)),2)+acc)
     }
    
    Math.sqrt(helper(l,0.0))
  }
  
  def vectorCalculationTailRecursive2(l:Array[Double]):Double= {

    val len= l.length
    def helper(index:Int,acc:Double):Double=
      if (index>=len) acc
        else helper(index+1,math.pow(math.sin(math.log(l(index))),2)+acc)
     
    Math.sqrt(helper(0,0.0))
  }
  
  def vectorCalculationUsingFold(l:Array[Double]):Double= {
    
    Math.sqrt(l.foldLeft(0.0)((x,y)=> x + math.pow(math.sin(math.log(y)),2)))
  }
  

  
  def vectorCalculationUsingParFold(l:scala.collection.parallel.mutable.ParArray[Double]):Double= {
    
    /*
    Math.sqrt(l.fold(0.0)((x,y)=> (math.pow(math.sin(math.log(y)),2))+x))
    * 
    * Doesn't lead to the expected result foldFun is not associative
    */
    
    Math.sqrt(l.map(x=>math.pow(math.sin(math.log(x)),2)).fold(0.0)((x,y)=> x + y))
  }
  
  def vectorCalculationUsingParMapAndReduce(l:scala.collection.parallel.mutable.ParArray[Double]):Double= {
    
     Math.sqrt(l.map(x=>math.pow(math.sin(math.log(x)),2)).reduce((x,y)=> x + y))
  }
  
  def vectorCalculationUsingRecursiveTask(l:Array[Double]):Double= {
    
    val t= new myRecursiveTask(0,l.length-1,l)
    t.fork
    Math.sqrt(t.join)
  }
}

class myRecursiveTask (var start:Int, end:Int, l:Array[Double]) extends RecursiveTask[Double]{
  
 def computeValue:Double={
 
     var result=0.0
     while (start<=end){
       result= result+math.pow(math.sin(math.log(l(start))),2)
       start+=1
     } 
   result
 }
 
 def compute:Double={
   
   if ((end-start) < 1000000) (computeValue)

   else {
     
     val mid:Int= start+((end-start)/2)
     val t1= new myRecursiveTask(mid+1,end,l)
     val t2= new myRecursiveTask(start,mid,l)
     pool.execute(t1)
     pool.execute(t2)
     (t1.join+t2.join)
   }
 }  
}