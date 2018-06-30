package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import streaming._

@RunWith(classOf[JUnitRunner])
class SimpleStreamTest extends FunSuite {

  
	trait TestStreams{ 

	  val s1= SimpleStream(1,2,3,4,5)
    val s2= SimpleStream(1,2,3,4,5,6)
    val s3= SimpleStream(1,3,4,5)
    val s4= SimpleStream(1,2,3)
    val s5= SimpleStream(1,2,3,3)
    val s6= SimpleStream(1, 1, 3)
    val s7= SimpleStream(1, 2, 3, 4)
	}
	
	
	test("equals Method") {
	  
		new TestStreams{
    	
			assert(s1===s1)
		  assert(!(s1==s2))
			assert(!(s1==s3))
			assert(!(s1==s4))
		}
	}
	
	test("List Method") {
	  
		new TestStreams{
    	
		  assert(s1.toString==="SimpleStream(1, ?)")	
			assert(s1.toList==List(1,2,3,4,5))
			assert(s1.toString==="SimpleStream(1, 2, 3, 4, 5)")    
		}
	}
	
	test("take Method") {
	  
		new TestStreams{
    	
		  val r= s1.take(3)
		  assert(s1.toString==="SimpleStream(1, ?)")	
		  val l= r.toList
		  assert(l== List(1,2,3))
 		  assert(s1.toString==="SimpleStream(1, 2, 3, ?)")	
		}
	}
	
	test("take Method 1") {
	  
		new TestStreams{
    	
		  val r= s1.take(3)
		  assert(s1.toString==="SimpleStream(1, ?)")	
		  val l= r.toList
		  assert(l== List(1,2,3))
 		  assert(s1.toString==="SimpleStream(1, 2, 3, ?)")	
		}
	}
	
	test("take Method 2") {
	  
		new TestStreams{
    	
		  val r= s1.take(5)
		  assert(s1.toString==="SimpleStream(1, ?)")	
		  val l= r.toList
		  assert(l== List(1,2,3,4,5))
 		  assert(s1.toString==="SimpleStream(1, 2, 3, 4, 5, ?)")
 		  val p= s1.take(7).toList
 		  assert(l== List(1,2,3,4,5))
 		  assert(s1.toString==="SimpleStream(1, 2, 3, 4, 5)")
		}
	}
	
  test("map Method") {
	  
		new TestStreams{
    	
		  val r= s1.map(x=>x+1)
		  assert(s1.toString==="SimpleStream(1, ?)")
		  assert(r.toString==="SimpleStream(2, ?)")
		  val x= r.take(3)
		  assert(s1.toString==="SimpleStream(1, ?)")
		  assert(r.toString==="SimpleStream(2, ?)")
		  val l= x.toList
		  assert(l===List(2,3,4))
		  assert(s1.toString==="SimpleStream(1, 2, 3, ?)")
		  assert(r.toString==="SimpleStream(2, 3, 4, ?)")
	    val y= r.take(5).toList
	    assert(y===List(2,3,4,5,6))
		  assert(s1.toString==="SimpleStream(1, 2, 3, 4, 5, ?)")	
		  assert(r.toString==="SimpleStream(2, 3, 4, 5, 6, ?)")	
	    val z= r.take(7).toList
	    assert(y===List(2,3,4,5,6))
		  assert(s1.toString==="SimpleStream(1, 2, 3, 4, 5)")	
		  assert(r.toString==="SimpleStream(2, 3, 4, 5, 6)")	
		}
	}
  
  test("filter Method") {
	  
		new TestStreams{
    	
		  val r= s1.map(x=>x+1).filter(_%2==0)
		  assert(s1.toString==="SimpleStream(1, ?)")
		  assert(r.toString==="SimpleStream(2, ?)")
		  val x= r.take(3)
		  assert(s1.toString==="SimpleStream(1, ?)")
		  assert(r.toString==="SimpleStream(2, ?)")
		  val l= x.toList
		  assert(l===List(2,4,6))
		  assert(s1.toString==="SimpleStream(1, 2, 3, 4, 5, ?)")
		  assert(r.toString==="SimpleStream(2, 4, 6, ?)")
	    val y= r.take(5).toList
	    assert(y===List(2,4,6))
		  assert(s1.toString==="SimpleStream(1, 2, 3, 4, 5)")	
		  assert(r.toString==="SimpleStream(2, 4, 6)")	
		}
	}
  
  test("apply Method 1") {
	  
		new TestStreams{
    	
		  val r= s2(2)
		  assert(r==3)
		  assert(s2.toString==="SimpleStream(1, 2, 3, ?)")
		}
  }
  
  test("apply Method 2") {
	  
		new TestStreams{
    	
		  val r= s2(5)
		  assert(r==6)
		  assert(s2.toString==="SimpleStream(1, 2, 3, 4, 5, 6, ?)")
		}
  }
  
  test("apply Method 3") {
	  
		new TestStreams{
    	
		  assertThrows[java.lang.Error]{
		    val r= s2(8)
		  }
		  assert(s2.toString==="SimpleStream(1, 2, 3, 4, 5, 6)")
		}
  }
  
  test("createStream Method 1") {
	  
		new TestStreams{
    	
		  val s= SimpleStream.createStreamRange(1,6)
		  assert(s.toString==="SimpleStream(1, ?)")
		  assert(s.toList===List(1, 2, 3, 4, 5, 6))
		  assert(s.toString==="SimpleStream(1, 2, 3, 4, 5, 6)")
		}
  }

  test("createStream Method 2") {
	  
		new TestStreams{
    	
		  val s= SimpleStream.createStreamRange(6,1)
		  assert(s.toString==="SimpleStream.Empty")
		}
  }

  
  test("fold Method") {
	  
		new TestStreams{
    	
		  val r1= s1.fold(0)((x,y)=>x+y)
		  assert(r1==15)
		  assert(s1.toString==="SimpleStream(1, 2, 3, 4, 5)")
		  val r2= s1.fold(0)((x,y)=>x-y)
		  assert(r2== -15)
		  assert(s1.toString==="SimpleStream(1, 2, 3, 4, 5)")
		}
  }
  
  test("createStream Method") {
	  
		new TestStreams{
    	
		  val r1= SimpleStream.createStream(4)(n=>n+2)
		  val r2= r1.take(8)
		  assert(r1.toString=="SimpleStream(4, ?)")
		  r2.toList
		  assert(r1.toString=="SimpleStream(4, 6, 8, 10, 12, 14, 16, 18, ?)")
		  val r3= r1.take(100).fold(0)((x,y)=>x+y)
		  assert(r3== 10300)
		}
  }
  
  test("createStreamGen Method") {
	  
		new TestStreams{
    	
		  val r1= SimpleStream.createStreamGen((1,List():List[Int]))(el=>((el._1+1,el._1::el._2), el._2.sum))
		  val r2= r1.take(8)
		  assert(r1.toString=="SimpleStream(0, ?)")
		  r2.toList
		  assert(r1.toString=="SimpleStream(0, 1, 3, 6, 10, 15, 21, 28, ?)")
		  val r3= r1.take(100).fold(0)((x,y)=>x+y)
		  assert(r3== 166650)
		}
  }
  
  test("startsWith Method 1") {
	  
		new TestStreams{
    	
		  val s=s1.startsWith(s4)
		  assert(s===true)
		  assert(s1.toString=="SimpleStream(1, 2, 3, ?)")
		}
  }
  
  test("startsWith Method 2") {
	  
		new TestStreams{
    	
		  val s=s1.startsWith(s5)
		  assert(s===false)
		  assert(s1.toString=="SimpleStream(1, 2, 3, 4, ?)")
		}
  }
  
  test("startsWith Method 3") {
	  
		new TestStreams{
    	
		  val s=s1.startsWith(s6)
		  assert(s===false)
		  assert(s1.toString=="SimpleStream(1, 2, ?)")
		}
  }
  
  test("createNaturalNumbersStream Method") {
	  
		new TestStreams{
    	
		  val n=StreamApps.createNaturalNumbersStream(1)
		  val s=n.take(6)
		  assert(s.toString=="SimpleStream(1, ?)")
		  val l=s.toList
		  assert(n.toString=="SimpleStream(1, 2, 3, 4, 5, 6, ?)")
		  assert(s.toString=="SimpleStream(1, 2, 3, 4, 5, 6)")

		  assert(l===List(1,2,3,4,5,6))
		}
  }
  
   test("createFibonacciStream Method") {
	  
		new TestStreams{
    	
		  val n=StreamApps.createFibonacciStream
		  val s=n.take(10)
		  assert(s.toString=="SimpleStream(0, ?)")
		  val l=s.toList
		  assert(n.toString=="SimpleStream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ?)")
		  assert(s.toString=="SimpleStream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)")

		  assert(l===List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
		}
  }
   
  test("createStreamOfStreams Method") {
	  
    new TestStreams{
		  val s=StreamApps.createStreamOfStreams(s7)
		  assert(s.toString==="SimpleStream(SimpleStream(1, ?), ?)")
		  val l= s.toList
		  assert(s.toString==="SimpleStream(SimpleStream(1, 2, 3, 4), SimpleStream(2, 3, 4), SimpleStream(3, 4), SimpleStream(4), SimpleStream.Empty)")
      assert(l== List(SimpleStream(1, 2, 3, 4), SimpleStream(2, 3, 4), SimpleStream(3, 4), SimpleStream(4), SimpleStream.empty))
    }
  }
}