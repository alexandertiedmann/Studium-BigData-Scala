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

}