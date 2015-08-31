package bot

import Dir._
import scala.util.Random
import Tile._
import math.{sqrt, abs}

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
  def move(input: Input): Dir =
    new BrigadierMove(this, input).move()
}

class BrigadierMove(bot: Brigadier, input: Input) {
  val game  = input.game
  val board = input.game.board
  val hero  = input.hero

  def move(): Dir = {
    println(s"Turn: ${game.turn}")
    val target = closest(hero.pos, foreignMines())
    val advance = toward(hero.pos, target)
    isWalkable(hero.pos.to(advance)) match {
      case true =>
        println(s"  advance ${advance}")
        advance
      case false =>
        println("  random")
        randomDir()
    }
  }

  def foreignMines(): List[Pos] =
    select((p, t) => t match {
      case Mine(heroId) => heroId match {
        case Some(heroId) => (heroId != hero.id)
        case None => true
      }
      case _ => false
    })

  def mines(): List[Pos] =
    select((p, t) => t match {
      case _:Mine => true
      case _ => false
    })

  def toward(origin: Pos, target: Pos): Dir = {
    List(North, East, South, West).sortBy{ dir =>
      distance(origin.to(dir), target)
    }.head
  }

  def distance(a: Pos, b: Pos): Double = {
    val diffx = abs(b.x - a.x)
    val diffy = abs(b.y - a.y)
    diffx + diffy
  }

  def closest(to: Pos, candidates: List[Pos]) =
    candidates.sortBy(distance(to, _)).head

  // Get all positions satisfying a predicate.
  def select(pred: (Pos, Tile) => Boolean): List[Pos] = {
    (0 until game.board.size).flatMap{ x =>
      (0 until game.board.size).map{ y =>
        val p = Pos(x, y)
        val z: (Pos, Tile) = (p, game.board.at(Pos(x, y)).get)
        z
      }
    }.filter(pred.tupled).map(_._1).toList
  }

  def isWalkable(p: Pos): Boolean =
    game.board.at(p).getOrElse(Wall) match {
      case Wall => false
      case _:Tile.Hero => false
      case Air => true
      case Tavern => true
      case _:Mine => true
    }

  def randomDir() = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      game.board at hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}
