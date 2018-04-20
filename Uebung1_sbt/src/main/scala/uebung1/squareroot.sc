def sqrt(x:Double): Double={
  def sqrtHelp(x:Double, estimate:Double): Double={
    if (isGoodEnough(estimate)) estimate
    else sqrtHelp(x, improve(estimate))
  }
  def isGoodEnough(estimate: Double): Boolean={
    if (Math.abs(estimate * estimate - x) > 0.00001) false
    else true
  }
  def improve(estimate: Double): Double={
    (x + (x / estimate)) / 2
  }
  sqrtHelp(x, 1)
}
print(sqrt(4))
print(sqrt(2))
print(sqrt(8))