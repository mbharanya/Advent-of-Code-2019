import scala.io.Source
import scala.util.Try

object Day2 extends App {
  val filename = "inputs/day_2"

  val instructions: List[Int] = Source.fromFile(filename).getLines.map(_.split(",").map(_.toInt)).flatten.toList

  val modified = instructions.updated(1, 12).updated(2, 2)
  val part1Result = getResult(modified)(0)

  println(s"Part 1: ${part1Result}")

  for (a <- 0 to 99 ) {
    for (b <- 0 to 99) {
      val modifiedInstructions = instructions.updated(1, a).updated(2, b)
      val res = Try(getResult(modifiedInstructions)(0)).toOption
      res match {
        case Some(19690720) => println(s"Part 2: pair ${a}, ${b}")
        case _ => ()
      }
    }
  }


  def getResult(instructions: List[Int], address: Int = 0): List[Int] = {
    val newAddress = address + 4
    val instruction = instructions.slice(address, newAddress)

    val code = instruction(0)
    //       val (value1, value2, position) = (instruction.lift(1), instruction.lift(2), instruction.lift(3))

    val result = code match {
      case 1 => getResult(
        instructions.updated(instruction(3), instructions(instruction(1)) + instructions(instruction(2))),
        newAddress
      )
      case 2 => getResult(
        instructions.updated(instruction(3), instructions(instruction(1)) * instructions(instruction(2))),
        newAddress
      )
      case 99 => instructions
      case unknown: Int => throw new RuntimeException(s"Unknown command ${unknown}")
    }
    result
  }

}
