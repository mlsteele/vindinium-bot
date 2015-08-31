package bot

import Dir._
import scala.util.Random
import Tile._
import math.sqrt

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
  def move(input: Input) = {
    val target = closest(input.hero.pos, mines(input))
    val advance = toward(input.hero.pos, target)
    isWalkable(input, input.hero.pos.to(advance)) match {
      case true => advance
      case false => randomDir(input)
    }
  }

  def mines(implicit input: Input): List[Pos] =
    select(input, ((p, t) => t match {
      case _:Mine => true
      case _ => false
    }))

  def toward(origin: Pos, target: Pos): Dir = {
    List(North, South, West, East).map{ dir =>
      (dir, distance(origin.to(dir), target))
    }.sortBy(_._2).head._1
  }

  def distance(a: Pos, b: Pos): Double = {
    val diffx = b.x - a.x
    val diffy = b.y - a.y
    sqrt(diffx * diffx + diffy + diffy)
  }

  def closest(to: Pos, candidates: List[Pos]) =
    candidates.sortBy(distance(to, _)).head

  // Get all positions satisfying a predicate.
  def select(input: Input, pred: (Pos, Tile) => Boolean): List[Pos] = {
    (0 until input.game.board.size).flatMap{ x =>
      (0 until input.game.board.size).map{ y =>
        val p = Pos(x, y)
        val z: (Pos, Tile) = (p, input.game.board.at(Pos(x, y)).get)
        z
      }
    }.filter(pred.tupled).map(_._1).toList
  }

  def isWalkable(implicit input: Input, p: Pos): Boolean =
    input.game.board.at(p).getOrElse(Wall) match {
      case Wall => false
      case _:Hero => false
      case Air => true
      case Tavern => true
      case _:Mine => true
    }

  def randomDir(input: Input) = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}
