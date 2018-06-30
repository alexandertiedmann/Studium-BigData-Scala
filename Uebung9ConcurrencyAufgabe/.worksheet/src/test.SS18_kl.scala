package test

object SS18_kl {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(74); 
  println("Welcome to the Scala worksheet");$skip(129); 
   def checkOrdering(a:Array[Int])=
    if (a.isEmpty) true
      else a.foldLeft((true,a.head))((x,y)=> ((x._1 && (x._2<y)),y));System.out.println("""checkOrdering: (a: Array[Int])Any""")}
}
