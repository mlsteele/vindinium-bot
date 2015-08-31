package bot

import scala.util.Try

object Test {
  def testBogus(unused: Boolean) = {
    println("stuff is going ok")
    assert(1==1)
  }

  def testBfs(unused: Boolean) = {
    val board_lines = List(
      "####################################",
      "################$-$-################",
      "##########@1[]##    ##[]@4##########",
      "##########  ##        ##  ##########",
      "######$-    ####    ####    $-######",
      "##########                ##########",
      "########                    ########",
      "######    ##  ########  ##    ######",
      "######  $-##  ########  ##$-  ######",
      "######  $-##  ########  ##$-  ######",
      "######    ##  ########  ##    ######",
      "########                    ########",
      "##########                ##########",
      "######$-    ####    ####    $-######",
      "##########  ##        ##  ##########",
      "##########@2[]##    ##[]@3##########",
      "################$-$-################",
      "####################################")
    val board = parseBoard(board_lines)
    // println(board)
    val start = Pos(8,4)
    val goal = Pos(12,6)
    val pred: (Pos => Boolean) = {pos => pos == goal}
    // val path = BrigadierMove(bot, input).bfs(start, pred)
    // println(path)
  }

  def parseBoard(board_lines: Seq[String]): Board = {
    val size = board_lines(0).length
    val tiles = board_lines.toVector.flatMap{_.grouped(2).map(parseTile).toVector}
    Board(size, tiles)
  }

  def parseTile(str: String): Tile = str.toList match {
    case List(' ', ' ') ⇒ Tile.Air
    case List('#', '#') ⇒ Tile.Wall
    case List('[', ']') ⇒ Tile.Tavern
    case List('$', x)   ⇒ Tile.Mine(int(x))
    case List('@', x)   ⇒ Tile.Hero(int(x) getOrElse sys.error(s"Can't parse $str"))
    case x              ⇒ sys error s"Can't parse $str"
  }

  private def int(c: Char): Option[Int] =
    Try(java.lang.Integer.parseInt(c.toString)).toOption

  def runTest(test: Boolean => Unit) = {
    try {
      println("Running test...")
      test(true)
      println("Test passed :).")
    }
    catch {
      case e: java.lang.AssertionError =>
        println("Test FAILED. (assertion)")
      case e: Exception =>
        println("Test FAILED.")
    }
  }

  def runAll() = {
    runTest(testBogus)
    runTest(testBfs)
  }
}
