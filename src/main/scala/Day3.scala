import scala.io.Source
import scala.util.Try

object Day3 extends App {

  case class Position(x: Int, y: Int) {
    def manhattenDistanceToCenter = Math.abs((0 - x) + (0 - y))

    def +(position: Position): List[Position] =
      Math.min(x, position.x) to (Math.max(x, position.x) + x)


      (Math.min(x, position.x) to (Math.max(x, position.x) + x))
        .flatMap(newX =>
          (Math.min(y, position.y) to (Math.max(y, position.y) + y)).map(newY => Position(newX, newY))).toList
  }
  // Position(5, 8) + Position(0, -5)


  sealed abstract class Command extends Product with Serializable

  object Command {

    final case class Right(amount: Int) extends Command

    final case class Left(amount: Int) extends Command

    final case class Up(amount: Int) extends Command

    final case class Down(amount: Int) extends Command

  }


  val filename = "inputs/day_3"
  val pattern = "([R|L|U|D])([0-9]+)".r

  val testData = List("R8,U5,L5,D3", "U7,R6,D4,L4")
  val testData1 = List("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")


  val instructionsPerWire = testData.map(_.split(",")).map(instructionsForOneWire => {
    instructionsForOneWire.map(instruction => {
      val pattern(direction, amount) = instruction
      direction match {
        case "R" => Command.Right(amount.toInt)
        case "L" => Command.Left(amount.toInt)
        case "U" => Command.Up(amount.toInt)
        case "D" => Command.Down(amount.toInt)
      }
    })
  }).toList


  def commandsToPos(commands: List[Command]): List[Position] = {
    var position = Position(0, 0)
    commands.flatMap(command => {
      val positions = getPosWithPrevious(command, position)
      position = positions(positions.length - 1)
      positions
    })
  }


  val coordinatesPerWire: List[List[Position]] = instructionsPerWire
    .map(oneWire => commandsToPos(oneWire.toList))

  def getPosWithPrevious(command: Command, prevPos: Position): List[Position] = {
    command match {
      case Command.Right(amount) => prevPos + Position(0, amount)
      case Command.Left(amount) => prevPos + Position(0, -amount)
      case Command.Up(amount) => prevPos + Position(amount, 0)
      case Command.Down(amount) => prevPos + Position(-amount, 0)
    }
  }

  val flattenedCoords = coordinatesPerWire.flatten
  val intersections = flattenedCoords.diff(flattenedCoords.distinct).distinct

  val closestDistance = intersections.map(_.manhattenDistanceToCenter).min

  println(s"closest distance is ${closestDistance}")

}
