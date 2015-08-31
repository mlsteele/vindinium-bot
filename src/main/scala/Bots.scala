package bot

import Dir._
import scala.util.Random
import Tile._

trait Bot {
  def move(input: Input): Dir
}

class RandomBot extends Bot {
  def move(input: Input) = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}

class Brigadier extends Bot {
  var last_dir = West

  def move(input: Input) = {
    last_dir = next()
    last_dir
  }

  def next() =
    last_dir match {
      case North => East
      case East => South
      case South => West
      case West => North
    }
}
