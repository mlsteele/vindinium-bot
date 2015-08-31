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

    // Stay at a tavern.
    if (isNeighboringTavern && canAffordBeer && !mostlyHealed) {
      println(s"Staying at tavern.")
      return visitTavern()
    }

    // Conquer a mine.
    bfs(isForeignMine).map{ pathToClosestForeignMine =>
      if (healthAfterPath(pathToClosestForeignMine) > 35) {
        println(s"Heading to attack mine.")
        return followPath(pathToClosestForeignMine)
      }
    }

    // Run to a tavern.
    bfs(isTavern).map{ pathToTavern =>
      println(s"Running to tavern.")
      return followPath(pathToTavern)
    }

    println(s"Nothing to do.")
    Stay
  }

  // Is there a neighboring tavern?
  def isNeighboringTavern(): Boolean = 
    hero.pos.neighbors.exists{ p =>
      board.at(p) match {
        case Some(Tavern) => true
        case _ => false
      }
    }

  def canAffordBeer(): Boolean =
    hero.gold >= 2

  def mostlyHealed(): Boolean =
    hero.life > 90

  def visitTavern(): Dir = {
    assert(isNeighboringTavern())
    val target = hero.pos.neighbors.filter{ p =>
      board.at(p) match  {
        case Some(Tavern) => true
        case _ => false
      }
    }.head
    toward(hero.pos, target)
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

  def bfs(pred: Pos => Boolean): Option[Path] =
    bfs(hero.pos, pred)

  // How much health would I have after following this path?
  // Currently only accounts for thirst.
  def healthAfterPath(path: Path): Int = {
    assert(path.head == hero.pos)
    assert(path.length > 0)
    hero.life - path.length + 1
  }

  // Next direction to walk to follow path.
  def followPath(path: Path): Dir = {
    assert(path.head == hero.pos)
    assert(path.length > 0)
    path.length match {
      case 1 => Stay
      case _ =>
        val dir = toward(hero.pos, path(1))
        println(s"going to ${path.last} by way of $dir")
        dir
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

  def isTavern(p: Pos): Boolean =
    board.at(p).get match {
      case Tavern => true
      case _      => false
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
