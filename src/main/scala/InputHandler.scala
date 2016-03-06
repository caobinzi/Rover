import scala.util.{Try, Success, Failure}
import scalaz._
import scalaz.Scalaz._
import concurrent._
import MyParserOps._

trait InputHandler[T] {
  def handle(s: RoverInput)(errorHandler: Throwable => Unit): T;
}

case object RoverInputHandler extends InputHandler[Option[(Rover, List[Command])]] {
  override def handle(s: RoverInput)(errorHandler: Throwable => Unit): Option[(Rover, List[Command])] = {
    s match {
      case RoverInput(_, _, Some(positionInputStr), Some(commandInputStr)) =>
        val result: Try[(Rover, List[Command])] = for {
          positionInput <- positionInputStr.parse[PositionInput]
          commandInput <- commandInputStr.parse[CommandInput]
        } yield {
          (Rover(positionInput.x, positionInput.y, positionInput.h), commandInput.steps)
        }
        result match {
          case Success(s) => s.some
          case Failure(e) => errorHandler(e); None
        }
      case _ => None
    }
  }
}

