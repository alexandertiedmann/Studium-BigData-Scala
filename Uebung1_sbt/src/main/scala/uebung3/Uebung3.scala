package uebung3

object Uebung3 {
  def main(args: Array[String]): Unit = {
    print(quersumme(123))
  }

  def quersumme(zahl:Int): Int={
    def querinner(zahl: Int, sum: Int): Int={
      if (zahl < 10) zahl+sum
      else querinner(zahl%10, sum)
    }
    querinner(zahl, 0)
  }
}
