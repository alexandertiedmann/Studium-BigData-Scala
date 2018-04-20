def isPrim(x: Int): Boolean={
  primHelper(x, 2, (Math.sqrt(x)+1).toInt)
  def primHelper(zahl:Int, counter: Int, max: Int): Boolean= {
    if (counter >= max) true
    else if (zahl % counter == 0) false
    primHelper(zahl, counter+1, max)
  }
}

val x: Int = 7
val y: Int = 6

print(isPrim(x).toString)
print(isPrim(y).toString)