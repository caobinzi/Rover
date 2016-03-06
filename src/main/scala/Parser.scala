import scalaz._
import Scalaz._
import scala.util.{Try, Success, Failure}

trait MyParser[A] { def compile(s: String): Try[A] }

trait MyParserOps {
  val value: String
  def parse[B: MyParser]:Try[B] = implicitly[MyParser[B]].compile(value)
}

object MyParserOps {

  implicit def toOps[B](s: String):MyParserOps = new MyParserOps {
    val value = s;
  }

  implicit val toPlateauInput = new MyParser[PlateauInput] {
    def compile(r: String) = Try {
      val p = """(\d+) (\d+)""".r
      r match {
        case p(a, b) => PlateauInput(a.toInt, b.toInt)
        case _       => throw new Exception(s"Invalid upper-right coordinates of the plateau : $r")
      }
    }
  }
  implicit val toHeading = new MyParser[Heading] {
    def compile(r: String):Try[Heading] = Try {
      r match {
        case "N" => North
        case "S" => South
        case "W" => West
        case "E" => East
        case _   => throw new Exception(s"Invalid Heading : $r")
      }
    }
  }

  implicit val toPositionInput = new MyParser[PositionInput] {
    def parseLine(r: String) = Try {
      val p = """(\d+) (\d+) (.+)""".r
      r match {
        case p(a, b, c) => (a, b, c)
        case _          => throw new Exception(s"Invalid Postion Input: $r")
      }
    }
    def compile(str: String):Try[PositionInput] = {
      for {
        (a, b, c) <- parseLine(str)
        heading <- c.parse[Heading]
      } yield PositionInput(a.toInt, b.toInt, heading)
    }
  }

  implicit val toCommandInput = new MyParser[CommandInput] {
    def toCommand(str: String) =
      str match {
        case "L" => CLeft
        case "R" => CRight
        case "M" => CMove
        case _   => throw new Exception(s"Invalid Command: $str")
      }
    def compile(str: String):Try[CommandInput]= Try { CommandInput(str.map(_.toString).toList.map(toCommand)) }
  }
}

