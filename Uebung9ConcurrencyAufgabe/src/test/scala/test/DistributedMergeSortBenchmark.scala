package test

import org.scalameter.api._
import conc._

object DistributedMergeSortBenchmark extends Bench.LocalTime  {
 
  val addSize= Gen.exponential("esize")(10000,10000000,10)
  val r= scala.util.Random
  val arrays: Gen[Array[Int]]= for (e <- addSize) yield ((1 to e).map(_=>r.nextInt(10000))).toArray

  
  performance of "Arrays" in {
    measure method "quickSort" in {
      using (arrays) in {
        l=>scala.util.Sorting.quickSort(l)
      }
    }
  }
 
  performance of "DistributedMergeSort" in {
    measure method "distributedMergeSort" in {
      using (arrays) in {
        l=>DistributedMergeSort.distributedMergeSort(l,0)
      }
    }
  }
  
  performance of "ScalaCollections" in {
    measure method "myQuicksort" in {
      using (arrays) in {
        l:Array[Int]=> myQuicksort(l,0,l.length)
        }
     }
  }
  
  performance of "ScalaCollections" in {
    measure method "sorted" in {
      using (arrays) in {
       l => l.sorted
      }
    }
  }
        
  def myQuicksort(a:Array[Int], from:Int, to:Int) = {

    def copy(a:Array[Int], start:Int, b:Array[Int]):Unit={
  
  		for (i <-0 to b.length-1) a(start+i)= b(i)
  	}
  	val elementsSorted= a.slice(from, to+1).sorted
  	copy(a, from, elementsSorted)
  }  
}