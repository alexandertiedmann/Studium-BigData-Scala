package conc
import common._
import java.util.concurrent._

object DistributedMergeSort{

 def main(args: Array[String]) = {
  
  val r= new scala.util.Random  
  val a= (for (i <- 1 to 20) yield r.nextInt(1000)).toArray   

    println("++++++++++++++++StartArray++++++++++++++++++++++++++++++++++")
    println(a.mkString(" "))
    println(a.sorted.mkString(" "))
    println("++++++++++++++++++++++++++++++++++++++++++++++++")  
    distributedMergeSort(a,2)
  }
  
  def distributedMergeSort(a:Array[Int], maxDepth:Int)={

    val exchangeArray= new Array[Int](a.length)
    val sort= new myRecursiveSort(a,exchangeArray, 0, a.length-1, 0, maxDepth)
    sort.fork
    sort.join
    if (maxDepth%2!=0) Array.copy(exchangeArray,0, a,0,exchangeArray.length)
  }
    
  class myRecursiveSort (a:Array[Int],exchangeArray:Array[Int],
      from:Int,until:Int, depth:Int, maxDepth:Int) extends RecursiveAction{
  
    override def compute: Unit= ???
      
  }
    
  def merge(src:Array[Int], dst:Array[Int], from:Int, mid:Int, until:Int)={


  }
    
   def myQuicksort(a:Array[Int], from:Int, to:Int) = {

    def copy(a:Array[Int], start:Int, b:Array[Int]):Unit={
  
  		for (i <-0 to b.length-1) a(start+i)= b(i)
  	}
  	val elementsSorted= a.slice(from, to+1).sorted
  	copy(a, from, elementsSorted)
  }
}