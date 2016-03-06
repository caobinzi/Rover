import scalaz._
import scalaz.Scalaz._
trait Heading
case object North extends Heading { override def toString = "N" }
case object South extends Heading { override def toString = "S" }
case object East extends Heading { override def toString = "E" }
case object West extends Heading { override def toString = "W" }

trait Command
case object CLeft extends Command
case object CRight extends Command
case object CMove extends Command
case class Rover(x: Int, y: Int, heading: Heading) { override def toString = s"${x} ${y} ${heading}" }

object Rover {
  def spinLeft(heading: Heading): Heading = {
    heading match {
      case North => West
      case West  => South
      case South => East
      case East  => North
    }
  }

  def spinRight(heading: Heading): Heading = {
    heading match {
      case West  => North
      case South => West
      case East  => South
      case North => East
    }
  }

  def move(maxX: Int, maxY: Int, command: Command,rover: Rover): Rover = {
    command match {
      case CLeft  => rover.copy(heading = spinLeft(rover.heading))
      case CRight => rover.copy(heading = spinRight(rover.heading))
      case CMove => rover.heading match {
        case West  => rover.copy(x = List(0, rover.x - 1).max)
        case South => rover.copy(y = List(0, rover.y - 1).max)
        case East  => rover.copy(x = List(maxX, rover.x + 1).min)
        case North => rover.copy(y = List(maxY, rover.y + 1).min)
      }
    }
  }
  def roverState(max: Int, maxY: Int)(command: Command)(checker: Rover => Boolean): State[Rover, Unit] = {
    import State._
    for {
      last <- get: State[Rover, Rover]
      rover = move(max, maxY, command, last)
      _ <- if (checker(rover)) put(rover) else put(last)
      

    } yield ()
  }

}

