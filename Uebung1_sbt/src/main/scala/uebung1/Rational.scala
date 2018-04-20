package uebung1

class Rational(numerator: Int, denominator: Int) {


  def this(denom: Int) = this(1, denom)

  override def toString: String = numerator + "/" + denominator

  require(denominator != 0, "Denominator mmuss != 0 sein") // wirft IllegalArgumentException
  println("Eine Rationale Zahl wurde erzeugt....") // ist Teil des Konstruktors

  def num: Int = numerator // damit numerator von aussen zugaenglich ist
  def denom: Int = denominator // damit denominator von aussen zugaenglich ist
  def value: Double = (num.toDouble / denom) // Konvertierung in eine Fliesskommazahl

  def max(x: Rational): Rational = {

    if (numerator / denominator < x.num / x.denom) this else x
  }

  def mul(x: Int): Rational = {
    new Rational(x * num, x * denom)
  }

  def add(r: Rational): Rational = {
    new Rational((num * r.denom) + (r.num * denom), denom * r.denom)
  }
  def sub(r: Rational): Rational = {
    this.add(r.neg)
  }

  def neg: Rational = {
    new Rational(numerator * (-1), denominator)
  }

  def ggT(a: Int, b:Int): Int ={
    if (b==0) a
    else ggT(b, a % b)
  }

  def reduceRat(): Rational ={
    val ggt = ggT(num, denom)
    new Rational(num / ggt, denom / ggt)
  }

}
