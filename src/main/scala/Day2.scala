import Day1.filename

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

object Day2 extends App {
  val filename = "inputs/day_2"

  val instructions: List[Int] = Source.fromFile(filename).getLines.map(_.split(",").map(_.toInt)).flatten.toList

  val modified = instructions.updated(1, 12).updated(2,2)


  println(getResult(modified))


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
