package streaming

trait SimpleStream[+A] {
  var evaluated= false  // only relevant for the toString method 
                        // flag that expresses  
	def isEmpty: Boolean
	def head: A
	def tail: ()=> SimpleStream[A]
	def toString_r:String= { if (isEmpty) "" else
	  if (evaluated) ", "+head+ tail().toString_r else ", ?"}
	
	def take(n: Int): SimpleStream[A] = this match {
    case Cons(h,t) if n > 1  => SimpleStream.cons(h, tail().take(n - 1))
    case Cons(h,t) if n == 1 => SimpleStream.cons(h, {evaluated=true ;SimpleStream.empty})
    case _                   => SimpleStream.empty
}
	
 def filter(p: A => Boolean): SimpleStream[A] =
    if (isEmpty) this
      else if (p(head)) SimpleStream.cons(head, tail().filter(p))
        else tail().filter(p)
        
 def map[O](f:A=>O):SimpleStream[O]= this match{
      case Cons(h,t) => SimpleStream.cons(f(h), tail().map(f))
      case _ => SimpleStream.empty
   }

 def toList: List[A] = {
    def extract(s: SimpleStream[A]): List[A] = s match {
      case Cons(h, t) =>  h :: extract(s.tail())
      case _          => Nil
    }
    extract(this)
 }
}

case object Empty extends SimpleStream[Nothing]{
  
  def isEmpty=true
  def head = throw new Error("Stream is empty")
	def tail = throw new Error("Stream is empty")
  override def toString= "SimpleStream.Empty"
}

case class Cons[+A](hd: A, tl: ()=> SimpleStream[A]) extends SimpleStream[A]{
 
  def tail= {evaluated=true;tl}
  def head= hd
  def isEmpty=false  
  override def toString= "SimpleStream("+head+{if (evaluated) tail().toString_r else ", ?"}+")" 
}

object SimpleStream {
	def cons[T](hd: T, tl: => SimpleStream[T]) = 	{
   lazy val tail = tl
   val head= hd
   new Cons[T](hd, ()=>tail )
	}
	val empty = Empty 
	def apply[A] (as:A*):SimpleStream[A] =
  		if (as.isEmpty) SimpleStream.empty else SimpleStream.cons(as.head, apply(as.tail: _*))
} 
/*
case class Cons[+A](hd: A, tl: ()=> SimpleStream[A]) extends SimpleStream[A]{
 
  lazy val tail= {evaluated=true;tl}
  def head= hd
  def isEmpty=false  
  override def toString= "SimpleStream("+head+", "+{if (evaluated) tail().toString_r else "?"}+")" 
}


object SimpleStream {
	def cons[T](hd: T, tl: => SimpleStream[T]) = 	{
   lazy val tail = tl 
   new Cons[T](hd, ()=>tail )
	}
	val empty = Empty 
	def apply[A] (as:A*):SimpleStream[A] =
  		if (as.isEmpty) SimpleStream.empty else SimpleStream.cons(as.head, apply(as.tail: _*))	
  		* 
  		*/