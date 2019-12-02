import scala.io.Source

object Day1 extends App{

  val filename = "inputs/day_1_1"
  val lines = Source.fromFile(filename).getLines.toList


  println(s"sum: ${lines.map(l => getFuel(l.toInt)).sum}")

  println(s"sum with rocket equation: ${lines.map(l => getFuelWithRocketEq(l.toInt)).sum}")

  def getFuel(mass: Int) = (mass / 3) - 2

  def getFuelWithRocketEq(mass: Int, total:Int = 0):Int = {
    val calculatedFuel = mass / 3 - 2
    if (calculatedFuel > 0)
      getFuelWithRocketEq(calculatedFuel, calculatedFuel + total)
    else
      total
  }

}
