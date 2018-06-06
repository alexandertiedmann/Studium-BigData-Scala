package list

object Application {
  
  /* Complete the given functions by practicing Higher Order Functions */
  
  /**************************************************************************
   * multiplyAndFilterEven should multiply all elements of the IntList by
   * the factor x and filter all element that are even
   ***************************************************************************/
  
  def multiplyAndFilterEven(l:IntList, x:Int):IntList= {
    l.mapInside(r => r * x).filterInside(r => r % 2 == 0)
  }
  
   /**************************************************************************
   * findMin should find the minimum of a list
   ***************************************************************************/
  
  def findMin(l:IntList):Int= {
    l.reduceLeft(_ min _)
  }
  
  /**************************************************************************
   * sumOddNumbers should sum up all odd numbers of a list
   ***************************************************************************/
  
  def sumOddNumbers(l:IntList)= {
    l.filterInside(r => r % 2 != 0).reduceLeft(_ + _)
  }
  
  /**************************************************************************
   * countEvenNumbers should count all numbers of a list that are even
   ***************************************************************************/
 
  def countEvenNumbers(l:IntList):Int= {
    l.filterInside(r => r % 2 == 0).foldLeftInside(0)((b,x) => b+1)
  }

  /**************************************************************************
   * groupby ubeungen
   ***************************************************************************/

  //mit mutableMap
  def groupByMutable[T, U](in : Iterable[T],fun:T=>U): Map[U, List[T]]= {
    val lmap = scala.collection.mutable.Map[U, List[T]]()
    for ( x <- in){
      val key = fun(x)
      lmap.update(key,x::lmap.getOrElse(key,List()))
    }
    lmap.toMap.mapValues(_.reverse)
  }

  //immutableMap
  def groupByImmutable[T, U](in : Iterable[T], fun:T=>U): Map[U, List[T]]= {
    var lmap = Map[U, List[T]]()
    for ( x <- in){
      val key = fun(x)
      lmap = lmap.updated(key,x::lmap.getOrElse(key,List()))
    }
    lmap.mapValues(_.reverse)
  }

  //mit multipler Parameterliste
  def groupBy[T, U](in : Iterable[T]) (fun:T=>U): Map[U, List[T]]= {
    var lmap = Map[U, List[T]]()
    for ( x <- in){
      val key = fun(x)
      lmap = lmap.updated(key,x::lmap.getOrElse(key,List()))
    }
    lmap.mapValues(_.reverse)
  }

  //main zum testen
  def main(args: Array[String]): Unit = {
    val l = List(3,5,3,5,4,2,6,8)
    println(groupByMutable(l, (x:Int)=> x%2 == 0))
    println(groupByImmutable(l, (x:Int)=> x%2 == 0))
    println(groupBy(l)((x: Int)=> x%2 == 0))
  }
}