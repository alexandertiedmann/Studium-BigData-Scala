package test

import org.scalameter.api._
import conc._

object PiCalculationBenchmark extends Bench.LocalTime  {
 
  val tries= Gen.exponential("nr")(10000,1000000,10)  
  
  performance of "PiCalculation" in {
    measure method "calculatePi" in {
      using (tries) in {
        l=>PiCalculation.calculatePi(l)
      }
    }
  }
  
  performance of "PiCalculation" in {
    measure method "CalculatePiDistributed" in {
      using (tries) in {
        l=>PiCalculation.calculatePiDistributed(l,2)
      }
    }
  }
  
  performance of "PiCalculation" in {
    measure method "calculatePiDistributedParColl" in {
      using (tries) in {
        l=>PiCalculation.calculatePiDistributedParColl(l,2)
      }
    }
  }
}