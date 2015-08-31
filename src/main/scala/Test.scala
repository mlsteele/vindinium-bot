package bot

object Test {
  def testBogus(unused: Boolean) = {
    println("stuff is going ok")
    assert(1==1)
  }

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
  }
}
