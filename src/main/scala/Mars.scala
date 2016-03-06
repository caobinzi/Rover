import scalaz._
import scalaz.Scalaz._
case class Mars(rovers: List[Rover], maxX: Int, maxY: Int) {
  def checker(s: Rover): Boolean = {
    !rovers.exists(
      x => x.x == s.x && x.y == s.y
    )
  }
  def runRover(rover: Rover, commands: List[Command]): Mars = {
    if (!checker(rover)) return this

    val result = commands.
      map(Rover.roverState(maxX, maxY)(_)(checker)).
      sequenceU.
      run(rover)

    this.copy(rovers = result._1 :: this.rovers)
  }
}
object Mars {
  def marsState(rover: Rover, commands: List[Command]): State[Mars, Unit] = {
    import State._
    for {
      last <- get: State[Mars, Mars]
      _ <- put(last.runRover(rover, commands))
    } yield ()
  }
}

