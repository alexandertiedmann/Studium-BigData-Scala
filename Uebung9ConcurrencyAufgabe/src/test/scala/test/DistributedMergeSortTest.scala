package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import conc._
import org.scalameter._

/**
 * @author hendrik

 */

@RunWith(classOf[JUnitRunner])
class DistributedMergeSortTest extends FunSuite {
  
  trait testElements{
     val r= new scala.util.Random
     val data20= (for (i <- 1 to 20) yield r.nextInt(1000)).toArray
     val data200= (for (i <- 1 to 20) yield r.nextInt(1000)).toArray
     val data2000= (for (i <- 1 to 20) yield r.nextInt(1000)).toArray
  }
  
  test("Elements depth=0"){
    
    new testElements{
      val data20s= data20.clone.sorted
      DistributedMergeSort.distributedMergeSort(data20,0)
      assert(data20s===data20)
      val data200s= data200.clone.sorted
      DistributedMergeSort.distributedMergeSort(data200,0)
      assert(data200s===data200)
      val data2000s= data2000.clone.sorted
      DistributedMergeSort.distributedMergeSort(data2000,0)
      assert(data2000s===data2000)
    }
  }

  test("Elements depth=1"){
    
    new testElements{
      val data20s= data20.clone.sorted
      DistributedMergeSort.distributedMergeSort(data20,1)
      assert(data20s===data20)
      val data200s= data200.clone.sorted
      DistributedMergeSort.distributedMergeSort(data200,1)
      assert(data200s===data200)
      val data2000s= data2000.clone.sorted
      DistributedMergeSort.distributedMergeSort(data2000,1)
      assert(data2000s===data2000)
    }
  }

  test("Elements depth=2"){
    
    new testElements{
      val data20s= data20.clone.sorted
      DistributedMergeSort.distributedMergeSort(data20,2)
      assert(data20s===data20)
      val data200s= data200.clone.sorted
      DistributedMergeSort.distributedMergeSort(data200,2)
      assert(data200s===data200)
      val data2000s= data2000.clone.sorted
      DistributedMergeSort.distributedMergeSort(data2000,2)
      assert(data2000s===data2000)
    }
  }
  
  test("Elements depth=3"){
    
    new testElements{
      val data20s= data20.clone.sorted
      DistributedMergeSort.distributedMergeSort(data20,3)
      assert(data20s===data20)
      val data200s= data200.clone.sorted
      DistributedMergeSort.distributedMergeSort(data200,3)
      assert(data200s===data200)
      val data2000s= data2000.clone.sorted
      DistributedMergeSort.distributedMergeSort(data2000,3)
      assert(data2000s===data2000)
    }
  }
  
  
  test("Elements depth=20"){
    
    new testElements{
      val data20s= data20.clone.sorted
      DistributedMergeSort.distributedMergeSort(data20,20)
      assert(data20s===data20)
      val data200s= data200.clone.sorted
      DistributedMergeSort.distributedMergeSort(data200,20)
      assert(data200s===data200)
      val data2000s= data2000.clone.sorted
      DistributedMergeSort.distributedMergeSort(data2000,20)
      assert(data2000s===data2000)
    }
  }
}