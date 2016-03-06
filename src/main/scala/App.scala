
import scala.util.{Try, Success, Failure}
import scalaz._
import scalaz.Scalaz._
import concurrent._
import MyParserOps._

object RoverApp extends App {

  def fromFile(f: String) = {
    val inputs: List[RoverInput] = scala.io.Source.fromFile(f).
      getLines.toList.
      map(Input.inputState).
      sequenceU.
      run(RoverInput(WaitingPlateau, None, None, None))._2

    inputs match {
      case RoverInput(_, Some(marsInput), None, None) :: rovers =>
        marsInput.parse[PlateauInput] match {
          case Success(s) =>
            rovers.flatMap(RoverInputHandler.handle(_)(parseErrorHandler)).
              map (x => Mars.marsState(x._1, x._2)).
              sequenceU.
              run(Mars(Nil, s.x, s.y))._1.
              rovers.
              reverse.
              foreach(println)

          case Failure(e) => parseErrorHandler(e)
        }
      case _ => println("error input")
    }

  }

  def parseErrorHandler(e: Throwable): Unit = println("Error Input:" + e)

  args.foreach(fromFile)

}
