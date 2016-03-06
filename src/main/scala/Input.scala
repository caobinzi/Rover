import scalaz._
import scalaz.Scalaz._

trait InputStatus
object WaitingPlateau extends InputStatus
object WaitingPosition extends InputStatus
object WaitingInstruction extends InputStatus

trait Input
case class PlateauInput(x: Int, y: Int) extends Input
case class PositionInput(x: Int, y: Int, h: Heading) extends Input
case class CommandInput(steps: List[Command]) extends Input

case class RoverInput(
  status:        InputStatus,
  plateauInput:  Option[String],
  positionInput: Option[String],
  commandInput:  Option[String]
)

object Input {
  def nextCommand(r: RoverInput, s: String): RoverInput = {
    r.status match {
      case WaitingPlateau     => r.copy(status = WaitingPosition, plateauInput = s.some, None, None)
      case WaitingPosition    => r.copy(status = WaitingInstruction, positionInput = s.some, commandInput = None)
      case WaitingInstruction => r.copy(status = WaitingPosition, commandInput = s.some)
    }
  }
  def inputState(s: String): State[RoverInput, RoverInput] = {
    import State._
    for {
      last <- get: State[RoverInput, RoverInput]
      cmd = nextCommand(last, s)
      next <- put(cmd)
    } yield cmd
  }
}

