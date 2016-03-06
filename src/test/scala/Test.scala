import org.scalatest._
import scala.util.{Try, Success, Failure}
import scalaz._
import scalaz.Scalaz._
import MyParserOps._

class MarsSpec extends FlatSpec with Matchers {
  def outputHandler(s: RoverInput): Option[String] = {
    s match {
      case RoverInput(_, Some(plateauInputStr), Some(positionInputStr), Some(commandInputStr)) =>
        val result: Try[Rover] = for {
          plateauInput <- plateauInputStr.parse[PlateauInput]
          positionInput <- positionInputStr.parse[PositionInput]
          commandInput <- commandInputStr.parse[CommandInput]
        } yield {
          commandInput.steps.
            map(Rover.roverState(plateauInput.x, plateauInput.y)(_)(_ => true)).
            sequenceU.
            run(Rover(positionInput.x, positionInput.y, positionInput.h))._1
        }
        result match {
          case Success(position) => position.toString.some
          case Failure(e)        => e.toString.some
        }
      case _ => None
    }
  }

  "A Rover from string inputs  " should "work successfully" in {
    val output: List[String] = List(
      "5 5",
      "1 2 N",
      "LMLMLMLMM",
      "3 3 E",
      "MMRMMRMRRM"
    ).map(Input.inputState).
      sequenceU.
      run(RoverInput(WaitingPlateau, None, None, None))._2.
      flatMap(outputHandler)
    output should be(List("1 3 N", "5 1 E"))
  }

  "A Rover from invalid plateau coordinates  " should "fail " in {
    val output: List[String] = List(
      "5 5 x",
      "1 2 N",
      "LMLMLMLMM",
      "3 3 E",
      "MMRMMRMRRM"
    ).map(Input.inputState).
      sequenceU.
      run(RoverInput(WaitingPlateau, None, None, None))._2.
      flatMap(outputHandler)
    output should be(
      List(
        "java.lang.Exception: Invalid upper-right coordinates of the plateau : 5 5 x",
        "java.lang.Exception: Invalid upper-right coordinates of the plateau : 5 5 x"
      )
    )
  }

  "A Rover from invalid position " should "fail " in {
    val output: List[String] = List(
      "5 5",
      "1 2 Ns",
      "LMLMLMLMM",
      "3 3 E",
      "MMRMMRMRRM"
    ).map(Input.inputState).
      sequenceU.
      run(RoverInput(WaitingPlateau, None, None, None))._2.
      flatMap(outputHandler)
    //info(output.toString)
    output should be(
      List(
        "java.lang.Exception: Invalid Heading : Ns",
        "5 1 E"
      )
    )
  }

  "A Rover from invalid command " should "fail " in {
    val output: List[String] = List(
      "5 5",
      "1 2 N",
      "LMLMLMLMMT",
      "3 3 E",
      "MMRMMRMRRMT"
    ).map(Input.inputState).
      sequenceU.
      run(RoverInput(WaitingPlateau, None, None, None))._2.
      flatMap(outputHandler)
    output should be(
      List(
        "java.lang.Exception: Invalid Command: T",
        "java.lang.Exception: Invalid Command: T"
      )
    )

  }

  "A Rover from  case classes  " should "work successfully" in {
    val output1: Rover = List(
      CLeft,
      CMove,
      CLeft,
      CMove,
      CLeft,
      CMove,
      CLeft,
      CMove,
      CMove
    ).
      map(Rover.roverState(5, 5)(_)(_ => true)).
      sequenceU.
      run(Rover(1, 2, North)).
      _1
    output1 should be(Rover(1, 3, North))

    val output2: Rover = List(
      CMove,
      CMove,
      CRight,
      CMove,
      CMove,
      CRight,
      CMove,
      CRight,
      CRight,
      CMove
    ).
      map(Rover.roverState(5, 5)(_)(_ => true)).
      sequenceU.
      run(Rover(3, 3, East))._1
    output2 should be(Rover(5, 1, East))

  }

}
