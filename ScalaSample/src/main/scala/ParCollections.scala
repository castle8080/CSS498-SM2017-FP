import scala.collection.parallel

object ParCollections {

  def timeit[T](f: => T): (T, Double) = {
    val st = System.nanoTime
    val result = f
    val et = System.nanoTime - st
    (result, et / 1000000.0)
  }

  def process() = {
    val items = (1 to 10000000).toArray
    for (x <- 1 to 10) {
      print("Result: " + timeit { items.par.reduce((a, b) => a + b) });
      print("Result: " + timeit { items.reduce((a, b) => a + b) });
    }
  }

  def main(args: Array[String]): Unit = {
    process
  }

}
