object Uebungen {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(61); 
  println("Welcome to the Scala worksheet")
  
  import streaming._;$skip(190); 
  
  
   
   def createStreamOfStreams[T](s:SimpleStream[T]):SimpleStream[SimpleStream[T]]=
     SimpleStream.createStreamGen(s)( (x:SimpleStream[T])=> (x,x.tail()));System.out.println("""createStreamOfStreams: [T](s: streaming.SimpleStream[T])streaming.SimpleStream[streaming.SimpleStream[T]]""");$skip(51); 
  
  
  val ss= SimpleStream(1,2,3,4,5,6,7,8,9,10);System.out.println("""ss  : streaming.SimpleStream[Int] = """ + $show(ss ));$skip(5); val res$0 = 
  ss;System.out.println("""res0: streaming.SimpleStream[Int] = """ + $show(res$0));$skip(15); val res$1 = 
  Empty==Empty;System.out.println("""res1: Boolean = """ + $show(res$1));$skip(138); 
   
  def getNthElement[T](n:Int, s:SimpleStream[T]):T = n match {
  
    case 1 => s.head
    case _ => getNthElement(n-1, s.tail())
  };System.out.println("""getNthElement: [T](n: Int, s: streaming.SimpleStream[T])T""");$skip(175); 
   
  def fold[S,T](foldFun:(S,T)=>S, base:S, s:SimpleStream[T]):S= s match {
  
 		 case Empty=> base
  	 case Cons(_,_)=> fold(foldFun, foldFun(base, s.head), s.tail())
  };System.out.println("""fold: [S, T](foldFun: (S, T) => S, base: S, s: streaming.SimpleStream[T])S""");$skip(128); 
  
 
 def createStream[T](base:T)(next:T=>T):SimpleStream[T]= {
 
    SimpleStream.cons(base,createStream(next(base))(next))
 };System.out.println("""createStream: [T](base: T)(next: T => T)streaming.SimpleStream[T]""");$skip(46); val res$2 = 
  
 createStream(1)((n:Int)=> n + 1).take(10);System.out.println("""res2: streaming.SimpleStream[Int] = """ + $show(res$2));$skip(70); 
 
 
 val fib=createStream((0,1))(x=>(x._2,x._1+x._2)).take(10).toList;System.out.println("""fib  : List[(Int, Int)] = """ + $show(fib ));$skip(71); val res$3 = 


 createStreamGen((0,1))((x:(Int,Int))=>((x._2,x._1+x._2),x._1+x._2));System.out.println("""res3: streaming.SimpleStream[Int] = """ + $show(res$3));$skip(180); 
  
 def createStreamGen[T,S](base:T)(next:T=>(T,S)):SimpleStream[S]= {
    val el= next(base)
    println(el)
    SimpleStream.cons(next(base)._2,createStreamGen(el._1)( next))
 };System.out.println("""createStreamGen: [T, S](base: T)(next: T => (T, S))streaming.SimpleStream[S]""");$skip(46); 
 
 val tt= SimpleStream(1,2,3,4,5,6,7,8,9,10);System.out.println("""tt  : streaming.SimpleStream[Int] = """ + $show(tt ));$skip(40); val res$4 = 
  fold((x:Int,y:Int)=>x+y,0,tt.take(5));System.out.println("""res4: Int = """ + $show(res$4));$skip(25); val res$5 = 
  
  getNthElement(3,ss);System.out.println("""res5: Int = """ + $show(res$5));$skip(6); val res$6 = 
   tt;System.out.println("""res6: streaming.SimpleStream[Int] = """ + $show(res$6));$skip(153); 
   
  def evaluateFunction[T](n:Int, s:SimpleStream[T]):SimpleStream[T]= n match {
  
  	case 0 => Empty
  	case n => evaluateFunction(n-1,s.tail())
  };System.out.println("""evaluateFunction: [T](n: Int, s: streaming.SimpleStream[T])streaming.SimpleStream[T]""");$skip(8); val res$7 = 
  
  ss;System.out.println("""res7: streaming.SimpleStream[Int] = """ + $show(res$7));$skip(76); val res$8 = 
  
  
 //  val q= evaluateFunction(10,ss)
 //  evaluateFunction(20,ss)
  ss;System.out.println("""res8: streaming.SimpleStream[Int] = """ + $show(res$8));$skip(40); 
    
     
   
   val s1= Stream(1,2,3);System.out.println("""s1  : scala.collection.immutable.Stream[Int] = """ + $show(s1 ));$skip(56); 
   val s2= Stream.cons(1, Stream.cons(2, Stream.empty));System.out.println("""s2  : Stream.Cons[Int] = """ + $show(s2 ));$skip(36); 
   val s3= 1 #:: 2 #:: Stream.empty;System.out.println("""s3  : scala.collection.immutable.Stream[Int] = """ + $show(s3 ));$skip(88); 
        
   def naturalNumbers(n:Int):Stream[Int] = Stream.cons(n, naturalNumbers(n+1));System.out.println("""naturalNumbers: (n: Int)Stream[Int]""");$skip(31); 
  
   val n= naturalNumbers(1);System.out.println("""n  : Stream[Int] = """ + $show(n ));$skip(27); val res$9 = 
   evaluateFunction(10,ss);System.out.println("""res9: streaming.SimpleStream[Int] = """ + $show(res$9));$skip(179); 
   
   
    
  
 def fibStream(n1:Int,n2:Int): Stream[Int] = (n1, n2) match {
   
   		case (0,1) => 0 #:: fibStream(1,0)
   		case _ => (n1+n2) #:: fibStream(n1+n2, n1)
   
   };System.out.println("""fibStream: (n1: Int, n2: Int)Stream[Int]""");$skip(43); 
  
  val f= fibStream(0,1).take(10).toList;System.out.println("""f  : List[Int] = """ + $show(f ))}
   //evaluateFunction(10,f)
   //f(15)
   //f.take(10).toList
  
}
