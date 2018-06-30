package test

import org.scalameter.api._
import conc._

object ConcurrencyExampleBenchmark extends Bench.LocalTime  {
 
  val addSize= Gen.exponential("esize")(10000,10000000,10)  
  val arrays: Gen[Array[Double]]= for (e <- addSize) yield ((1 to e).map(_.toDouble)).toArray
  val p_arrays: Gen[scala.collection.parallel.mutable.ParArray[Double]]= 
    for (e <- addSize) yield ((1 to e).map(_.toDouble)).toArray.par
/*
  performance of "ConcurrencyExample" in {
    measure method "vectorCalculationImparative" in {
      using (arrays) in {
        l=>ConcurrencyExample.vectorCalculationImparative(l)
      }
    }
  }
  /*
  //  Exception: java.lang.OutOfMemoryError: Java heap space
    performance of "ConcurrencyExample" in {
    measure method "vectorCalculationRecursive" in {
      using (arrays) in {
        l=>ConcurrencyExample.vectorCalculationRecursive(l)
      }
    }
  }*/
  
  
  
  performance of "ConcurrencyExample" in {
    measure method "vectorCalculationTailRecursive" in {
      using (arrays) in {
        l=>ConcurrencyExample.vectorCalculationTailRecursive(l)
      }
    }
  }
  
  performance of "ConcurrencyExample" in {
    measure method "vectorCalculationTailRecursive2" in {
      using (arrays) in {
        l=>ConcurrencyExample.vectorCalculationTailRecursive2(l)
      }
    }
  }
  
  */
  performance of "ConcurrencyExample" in {
    measure method "vectorCalculationUsingFold" in {
      using (arrays) in {
        l=>ConcurrencyExample.vectorCalculationUsingFold(l)
      }
    }
  }
  
  performance of "ConcurrencyExample" in {
    measure method "vectorCalculationUsingParFold" in {
      using (p_arrays) in {
        l=>ConcurrencyExample.vectorCalculationUsingParFold(l)
      }
    }
  }
  
  performance of "ConcurrencyExample" in {
    measure method "vectorCalculationUsingParMapAndReduce" in {
      using (p_arrays) in {
        l=>ConcurrencyExample.vectorCalculationUsingParMapAndReduce(l)
      }
    }
  }
  
  performance of "ConcurrencyExample" in {
    measure method "vectorCalculationUsingRecursiveTask" in {
      using (arrays) in {
        l=>ConcurrencyExample.vectorCalculationUsingRecursiveTask(l)
      }
    }
  } 
}