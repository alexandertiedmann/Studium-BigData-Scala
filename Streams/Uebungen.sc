object Uebungen {
  println("Welcome to the Scala worksheet")
  
  import streaming._
  
  
   
   def createStreamOfStreams[T](s:SimpleStream[T]):SimpleStream[SimpleStream[T]]=
     SimpleStream.createStreamGen(s)( (x:SimpleStream[T])=> (x,x.tail()))
  
  
  val ss= SimpleStream(1,2,3,4,5,6,7,8,9,10)
  ss
  Empty==Empty
   
  def getNthElement[T](n:Int, s:SimpleStream[T]):T = n match {
  
    case 1 => s.head
    case _ => getNthElement(n-1, s.tail())
  }
   
  def fold[S,T](foldFun:(S,T)=>S, base:S, s:SimpleStream[T]):S= s match {
  
 		 case Empty=> base
  	 case Cons(_,_)=> fold(foldFun, foldFun(base, s.head), s.tail())
  }
  
 
 def createStream[T](base:T)(next:T=>T):SimpleStream[T]= {
 
    SimpleStream.cons(base,createStream(next(base))(next))
 }
  
 createStream(1)((n:Int)=> n + 1).take(10)
 
 
 val fib=createStream((0,1))(x=>(x._2,x._1+x._2)).take(10).toList


 createStreamGen((0,1))((x:(Int,Int))=>((x._2,x._1+x._2),x._1+x._2))
  
 def createStreamGen[T,S](base:T)(next:T=>(T,S)):SimpleStream[S]= {
    val el= next(base)
    println(el)
    SimpleStream.cons(next(base)._2,createStreamGen(el._1)( next))
 }
 
 val tt= SimpleStream(1,2,3,4,5,6,7,8,9,10)
  fold((x:Int,y:Int)=>x+y,0,tt.take(5))
  
  getNthElement(3,ss)
   tt
   
  def evaluateFunction[T](n:Int, s:SimpleStream[T]):SimpleStream[T]= n match {
  
  	case 0 => Empty
  	case n => evaluateFunction(n-1,s.tail())
  }
  
  ss
  
  
 //  val q= evaluateFunction(10,ss)
 //  evaluateFunction(20,ss)
  ss
    
     
   
   val s1= Stream(1,2,3)
   val s2= Stream.cons(1, Stream.cons(2, Stream.empty))
   val s3= 1 #:: 2 #:: Stream.empty
        
   def naturalNumbers(n:Int):Stream[Int] = Stream.cons(n, naturalNumbers(n+1))
  
   val n= naturalNumbers(1)
   evaluateFunction(10,ss)
   
   
    
  
 def fibStream(n1:Int,n2:Int): Stream[Int] = (n1, n2) match {
   
   		case (0,1) => 0 #:: fibStream(1,0)
   		case _ => (n1+n2) #:: fibStream(n1+n2, n1)
   
   }
  
  val f= fibStream(0,1).take(10).toList
   //evaluateFunction(10,f)
   //f(15)
   //f.take(10).toList
  
}