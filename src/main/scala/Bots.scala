package bot

import Dir._
import scala.util.Random
import collection.mutable
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
  type Path = List[Pos]

  val game  = input.game
  val board = input.game.board
  val hero  = input.hero

  def move(): Dir = {
    println(s"Turn: ${game.turn}")
    val path = bfs(hero.pos, isForeignMine)
    path match {
      case None =>
        println("no path to goal")
        Stay
      case Some(path) => followPath(path)
    }
  }

  // Do a BFS search from `start` towards any place satisfying `pred`.
  // Returns the path from `start` to the endpoint including the endpoint.
  def bfs(start: Pos, pred: Pos => Boolean): Option[Path] = {
    val queue = mutable.Queue[Path]()
    val visited = mutable.Set[Pos]()
    queue.enqueue(List(start))
    while (!queue.isEmpty) {
      val path = queue.dequeue()
      val pos = path.head
      if (pred(pos)) { return Some(path) }
      visited.add(pos)
      // Add walkable unvisited neighbors to queue
      val newpaths = pos.neighbors
        .filter(isWalkable)
        .filter{!visited.contains(_)}
        .map{ neighbor => path :+ neighbor }
      newpaths.map(queue.enqueue(_))
    }
    // No path to any goal was found.
    return None
  }

  def followPath(path: Path): Dir = {
    assert(path.head == hero.pos)
    assert(path.length > 0)
    path.length match {
      case 1 => Stay
      case _ => toward(hero.pos, path(1))
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

  def isForeignMine(p: Pos): Boolean =
    board.at(p).get match {
      case Mine(Some(heroId)) => heroId != hero.id
      case Mine(_) => true
      case _ => false
    }

  def mines(): List[Pos] =
    select((p, t) => t match {
      case _:Mine => true
      case _ => false
    })

  def toward(origin: Pos, target: Pos): Dir = {
    List(North, East, South, West, Stay).sortBy{ dir =>
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
