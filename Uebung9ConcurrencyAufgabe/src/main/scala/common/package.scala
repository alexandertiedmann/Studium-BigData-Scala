

import java.util.concurrent._
import scala.util.DynamicVariable

package object common {

  def ??? : Nothing = throw new Error("an implementation is missing")  
  type ??? = Nothing
  type *** = Any
  
  val pool = new ForkJoinPool(64)
  
  def thread(body: => Unit):Thread= {
    val t= new Thread{
      override def run= body
    }
    t.start
    t
  }
}

