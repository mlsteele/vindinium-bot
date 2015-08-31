package bot
import sys.process._

object Main {

  val bot: Bot = new Brigadier
  val open_browser = true

  def main(args: Array[String]) = makeServer match {
    case Left(error) ⇒ println(error)
    case Right(server) ⇒ args match {
      case Array() ⇒
        training(server, _.training(100))
      case Array("arena") ⇒
        arena(server, Int.MaxValue)
      case Array("arena", games) ⇒
        arena(server, int(games))
      case Array("training", turns) ⇒
        training(server, _.training(int(turns)))
      case Array("training", turns, map) ⇒
        training(server, _.training(int(turns), Some(map)))
      case Array("test") ⇒
        Test.runAll()
      case a ⇒ println("Invalid arguments: " + a.mkString(" "))
    }
  }

  def arena(server: Server, games: Int) {
    @annotation.tailrec
    def oneGame(it: Int) {
      println(s"[$it/$games] Waiting for pairing...")
      val input = server.arena
      println(s"[$it/$games] Start arena game ${input.viewUrl}")
      if (open_browser) {
        s"google-chrome ${input.viewUrl}" !
      }
      steps(server, input)
      println(s"\n[$it/$games] Finished arena game ${input.viewUrl}")
      if (it < games) oneGame(it + 1)
    }
    failsafe {
      oneGame(1)
    }
  }

  def training(server: Server, boot: Server ⇒ Input) {
    failsafe {
      val input = boot(server)
      println("Training game " + input.viewUrl)
      if (open_browser) {
        s"google-chrome ${input.viewUrl}" !
      }
      steps(server, input)
      println(s"\nFinished training game ${input.viewUrl}")
    }
  }

  def steps(server: Server, input: Input) {
    failsafe {
      step(server, input)
    }
  }

  def failsafe(action: ⇒ Unit) {
    try {
      action
    }
    catch {
      case e: scalaj.http.HttpException ⇒ println(s"\n[${e.code}] ${e.body}")
      case e: Exception                 ⇒ println(s"\n$e")
    }
  }

  @annotation.tailrec
  def step(server: Server, input: Input) {
    if (!input.game.finished) {
      print(".")
      step(server, server.move(input.playUrl, bot move input))
    }
  }

  def makeServer = (
    Option(System.getProperty("server")) getOrElse "http://vindinium.org/",
    getKey
  ) match {
      case (_, None)  ⇒ Left("Specify the user key with -Dkey=mySecretKey")
      case (url, Some(key)) ⇒ Right(new Server(url + "/api", key))
    }

  def getKey: Option[String] = {
    Option[String](System.getProperty("key")).orElse(getKeyFromFile)
  }

  def getKeyFromFile: Option[String] = {
    val source = io.Source.fromFile("secrets.txt")
    try {
      val keyEntry = "key: *(.*)".r
      for (line <- source.getLines) {
        line match {
          case keyEntry(key) => return Some(key)
          case _ =>
        }
      }
    } finally {
      source.close()
    }
    None
  }

  def int(str: String) = java.lang.Integer.parseInt(str)
}
