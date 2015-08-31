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
      case Some(path) =>
        val dir = followPath(path)
        println(s"going to ${path.last} by way of $dir")
        dir
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
      val pos = path.last
      if (pred(pos)) { return Some(path) }
      visited.add(pos)
      // Add neighbors to queue:
      val newpaths = pos.neighbors
        // Must be unvisited
        .filter{!visited.contains(_)}
        // Must be on the board
        .filter{board.at(_).isDefined}
        // Must be walkable OR a goal
        .filter(p => pred(p) || isWalkable(p))
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

  // Get a list of the positions of all mines not owned by me.
  def foreignMines(): List[Pos] = select(isForeignMine)

  def isForeignMine(p: Pos): Boolean =
    board.at(p).get match {
      case Mine(Some(heroId)) => heroId != hero.id
      case Mine(_) => true
      case _ => false
    }

  // Get a list of the positions of all mines
  def mines(): List[Pos] =
    select(pos => board.at(pos).get match {
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
  def select(pred: Pos => Boolean): List[Pos] = {
    (0 until game.board.size).flatMap{ x =>
      (0 until game.board.size).map{ y =>
        Pos(x, y)
      }
    }.filter(pred).toList
  }

  def isWalkable(p: Pos): Boolean =
    game.board.at(p).getOrElse(Wall) match {
      case Air         => true
      case Wall        => false
      case Tavern      => false
      case _:Tile.Hero => false
      case _:Mine      => false
    }

  def randomDir() = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      game.board at hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}
