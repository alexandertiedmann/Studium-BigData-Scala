object tests {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(58); 
  println("Welcome to the Scala worksheet")
  import org.scalameter.api._;$skip(158); 
 

  //val sizes: Gen[Int] = Gen.range("size")(300000, 1500000, 300000)
  
  val sizes: Gen[Int] = Gen.range("size")(20, 50, 2);System.out.println("""sizes  : org.scalameter.api.Gen[Int] = """ + $show(sizes ));$skip(57); 
    
   val ranges= for (size<-sizes) yield 0 until size;System.out.println("""ranges  : org.scalameter.Gen[scala.collection.immutable.Range] = """ + $show(ranges ));$skip(51); 
   val nr= for (range <-ranges) yield range.toList;System.out.println("""nr  : org.scalameter.Gen[List[Int]] = """ + $show(nr ));$skip(24); 
   val i=ranges.dataset;System.out.println("""i  : Iterator[org.scalameter.Parameters] = """ + $show(i ));$skip(18); 
   val p1= i.next;System.out.println("""p1  : org.scalameter.Parameters = """ + $show(p1 ));$skip(18); 
   val p2= i.next;System.out.println("""p2  : org.scalameter.Parameters = """ + $show(p2 ));$skip(18); 
   val p3= i.next;System.out.println("""p3  : org.scalameter.Parameters = """ + $show(p3 ));$skip(37); 
      
   val x1=ranges.generate(p1);System.out.println("""x1  : scala.collection.immutable.Range = """ + $show(x1 ));$skip(30); 
   val x2=ranges.generate(p2);System.out.println("""x2  : scala.collection.immutable.Range = """ + $show(x2 ));$skip(30); 
   val x3=ranges.generate(p1);System.out.println("""x3  : scala.collection.immutable.Range = """ + $show(x3 ));$skip(15); 
   println(x1);$skip(15); 
   println(x2);$skip(15); 
   println(x3)}
   
}
