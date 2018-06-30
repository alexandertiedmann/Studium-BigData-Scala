

object Sorter {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(61); 
  println("Welcome to the Scala worksheet");$skip(704); 
  
  
  
   def merge(src:Array[Int], dst:Array[Int], from:Int, mid:Int, until:Int)={
    
    var left= from
    var right= mid
    for (i<- 0 to (until-from)) {
      
      this match{
      
        case _ if (left>=mid) => println("x");dst(from+i)= src(right); right+=1; println("left:"+(left)+" - "+src(right-1) )
        case _ if (right>until) => println("y");dst(from+i)= src(left); left+=1; println("right:"+(right)+" - "+src(left-1))
        case _ => println("r-"+right+" - "+(from+i));if (src(left)<src(right)) {dst(from+i)= src(left); left+=1; println(src(left-1)+"<"+src(right))}
        						else {dst(from+i)= src(right); right+=1;println(src(left)+">"+src(right-1))}
      }
    }
  };System.out.println("""merge: (src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int)Unit""");$skip(37); 
  val a= Array(19,4,2,6,8,1,7,26,29);System.out.println("""a  : Array[Int] = """ + $show(a ));$skip(34); 
  val b= new Array[Int](a.length);System.out.println("""b  : Array[Int] = """ + $show(b ));$skip(40); 
  val c= Array(1,4,27,36,8,12,17,26,28);System.out.println("""c  : Array[Int] = """ + $show(c ));$skip(20); 
  merge (a,b,2,5,8);$skip(3); val res$0 = 
 b;System.out.println("""res0: Array[Int] = """ + $show(res$0));$skip(71); 
                                                  
  merge (c,b,0,4,8);$skip(7); val res$1 = 
  
  b;System.out.println("""res1: Array[Int] = """ + $show(res$1));$skip(278); 
  
  
   
  
  
   def quicksort(a:Array[Int], from:Int, to:Int) = {
   
  	def copy(a:Array[Int], start:Int, b:Array[Int]):Unit={
  
  		for (i <-0 to b.length-1) a(start+i)= b(i)
  	}
  	
  	val elementsSorted= a.slice(from, to).sorted
  	copy(a, from, elementsSorted)
  
  };System.out.println("""quicksort: (a: Array[Int], from: Int, to: Int)Unit""");$skip(22); 
  
  quicksort(a,5,9);$skip(4); val res$2 = 
  a;System.out.println("""res2: Array[Int] = """ + $show(res$2));$skip(46); 
  
  val t= Array(19,4,21,26,83,12,71,2,29,4);System.out.println("""t  : Array[Int] = """ + $show(t ));$skip(20); 
  quicksort(t,5,10);$skip(4); val res$3 = 
  t;System.out.println("""res3: Array[Int] = """ + $show(res$3))}
}
