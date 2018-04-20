package uebung3

object Uebung3 {
  def main(args: Array[String]): Unit = {
    print(quersumme(123))
    print(fibo(100))
  }

  def quersumme(zahl:Int): Int={
    def querinner(zahl: Int, sum: Int): Int={
      if (zahl/10 == 0) zahl+sum
      else querinner(zahl/10, sum+zahl%10)
    }
    querinner(zahl, 0)
  }

  def fibo(zahl: Int): BigInt ={
    def innerfibo(zahl1: BigInt, zahl2:BigInt, counter:BigInt): BigInt={

    }

  }
}
