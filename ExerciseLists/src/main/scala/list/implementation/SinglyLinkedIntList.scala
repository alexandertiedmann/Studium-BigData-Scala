package list.implementation

import list.traits.IntList

/**
  * A companion object for the singly linked list.
  * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
  * which results in Cons(1,Cons(2,Cons(3,Empty))))
  */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
    * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
    * This particular implementation of it is also a variadic function, i.e.
    * a function which accepts one or more arguments of the same type (integers) as parameters.
    */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }
}

abstract class SinglyLinkedIntList extends IntList {

  override def prepend(elem: Int): IntList = Cons(elem, this)

  override def size: Int = {
    this match {
      case Empty => 0
      case _ => 1 + tail.size
    }
  }

  /** ------------------------------------------
    *
    * Exercise 5
    *
    * ------------------------------------------ */


  override def map(mapFunc: Int => Int): IntList = {
    this match {
      case Empty => Empty
      case Cons(head, tail) => Cons(mapFunc(head), tail.map(mapFunc))
    }
  }

  override def filter(filterFunc: Int => Boolean): IntList = {
    this match {
      case Empty => Empty
      case Cons(head, tail) if filterFunc(head) => Cons(head, tail.filter(filterFunc))
      case Cons(head, tail) => tail.filter(filterFunc)
    }
  }

  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => initial
      case Cons(head, tail) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
    }
  }

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => throw new IllegalArgumentException("Empty List")
      case Cons(v, Empty) => v
      case Cons(v1, Cons(v2, tail)) => Cons(reduceFunc(v1, v2), tail).reduceLeft(reduceFunc)
    }
  }

  /** ------------------------------------------
    *
    * Assignment 1
    *
    * ------------------------------------------ */

  override def forAll(predicateFunc: Int => Boolean): Boolean = {
    def innerCheck(predecateFunc: Int => Boolean, elem: IntList): Boolean = {
      predicateFunc(elem.head) match {
        case true =>
          if (elem.tail == Empty) true
          else innerCheck(predecateFunc, elem.tail)
        case false => false
      }
    }

    innerCheck(predicateFunc, this)
  }

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => initial
      case Cons(head, tail) => reduceFunc(head, tail.foldRight(initial)(reduceFunc))
    }
  }

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = {
    tail match {
      case Empty => head
      case _ => reduceFunc(head, tail.reduceRight(reduceFunc))
    }
  }

  override def insertionSort: IntList = {
    this match {
      case Empty => Empty
      case Cons(head, tail) => (tail.insertionSort).insertSorted(head)
    }
  }

  override def insertSorted(elem: Int): IntList = {
    this match {
      case Empty => Cons(elem, Empty)
      case Cons(head, tail) if (head >= elem) => Cons(elem, Cons(head, tail))
      case Cons(head, tail) => Cons(head, tail.insertSorted(elem))
    }
  }

  override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A = {
    this match {
      case Empty => initial
      case Cons(head, tail) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
    }
  }
}