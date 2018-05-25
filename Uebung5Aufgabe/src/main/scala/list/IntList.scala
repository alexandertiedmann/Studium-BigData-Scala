package list

abstract class IntList {

  def isEmpty:Boolean
  def head:Int
  def tail:IntList
  def nth(index:Int):Int
  def contains(elem:Int):Boolean
  def insert(elem:Int):IntList
  def insertSorted(elem:Int):IntList
  def delete(elem:Int):IntList
  def deleteAll(elem:Int):IntList
  def insertionSortI:IntList

  def insertSO(elem:Int):IntList= this match{
  
	  case Empty=>new Cons(elem, Empty)
	  case Cons(head,tail) if (head>=elem) => new Cons(elem,new Cons(head,tail))
	  case Cons(head,tail) => new Cons(head, tail.insertSO(elem))
  }
  
  def insertionSort:IntList= this match{
    
    case Empty => Empty
    case Cons(head, tail) => (tail.insertionSort).insertSO(head)   
  }
     
  def prefix(l:IntList):IntList= 
    l match {
    case Empty => this
    case Cons(head, tail) => new Cons(head,this.prefix(tail))
  }

  def flip:IntList= {
     this match {
       case Empty => Empty
       case Cons(head, tail)=> new Cons(head,Empty).prefix(tail.flip)
     }
  }

  /*------------------------------------------------------------------------------*/
  /* 													Exercise 5		
   * 
   * Implement the given function in two ways:
   * - use the base class IntList
   * - use the derived classes Cons and Empty																			*/
  /*------------------------------------------------------------------------------*/

  /*
   * 
   * Implement a function map, that applies a function func to all elements of the list 
   * and returns a list containing the new values.   
   */
  
  def map(mapFun:(Int)=>Int): IntList
  
  def mapInside(mapFun:(Int)=>Int): IntList= {
    this match {
      case Empty => Empty
      case Cons(head, tail) => Cons(mapFun(head), tail.mapInside(mapFun))
    }
  }
  
  /*------------------------------------------------------------------------------
   * 
   * Implement a function filter that filters the list regarding a given 
   * predicate and returns a new list containing all elements that fulfill the predicate.
   * 
   * */
  
  def filter(filterFun:Int=>Boolean):IntList
  
  def filterInside(filterFun:Int=>Boolean):IntList = {
    this match {
      case Empty => Empty
      case Cons (head,tail) =>
        if (filterFun(head))
          Cons(head, tail.filterInside(filterFun))
        else
          tail.filterInside(filterFun)
    }
  }

  /*------------------------------------------------------------------------------
   * 
   * Implement a function foldLeft that aggregates a list from left to right starting with a base element. 
   * Use the following order: op(... (op(op(base, a0), a1) ...), an)
   * 
   ------------------------------------------------------------------------------ */

  def foldLeft(base:Int)(reduce:(Int,Int)=>Int):Int
  
  def foldLeftInside(base:Int)(reduceFun:(Int,Int)=>Int):Int = {
    this match{
      case Empty => base
      case Cons(head,tail) => tail.foldLeftInside(reduceFun(base, head))(reduceFun)
    }
  }
  
/*------------------------------------------------------------------------------
 * 
 * Implement a function foldRight that aggregates a list from right to left starting with the last element 
 * that need to be combined with his predecessor. Use the following order: 
 * op(a0 op(a1, op(..., op(an, an-1)...)))
 * The function reduce has no base element.
 * 
 * ------------------------------------------------------------------------------*/

  def foldRight(base:Int)(reduce:(Int,Int)=>Int):Int
  
  def foldRightInside(base:Int)(reduceFun:(Int,Int)=>Int):Int = {
    this match {
      case Empty => base
      case Cons(head, tail) => reduceFun(head, tail.foldRightInside(base)(reduceFun))
    }
  }
  
 /*------------------------------------------------------------------------------
   * 
   *  Implement a function reduceLeft hat aggregates a list from left to right starting 
   * with the first element that need to be combined with the second element. 
   * Use the following order: op(... (op(op(a0,a1), a2) ...), an). 
   * The function reduce has no base element.
   *     * 
   * ------------------------------------------------------------------------------*/

  def reduceLeft(redFun:(Int,Int)=>Int):Int= {
    this match {
      case Empty => throw new NoSuchElementException("Error")
      case Cons(head, tail) => tail.foldLeft(head)(redFun)
    }
  }
  
  def reduceRight(redFun:(Int,Int)=>Int):Int= {
    this match {
      case Empty => throw new NoSuchElementException("Error")
      case Cons(head, tail) => tail.flip.foldRight(head)(redFun)
    }
  }
}