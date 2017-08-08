
import scala.collection.LinearSeq
import scala.collection.immutable.{Vector, List}
import scala.collection.mutable.ArrayBuffer

object ListAccess {

    def runVector(counts: Array[Int]) {
        for (count <- counts) {
            val items = Vector.fill(count)(0)
            val st = System.nanoTime
            val counts = (1 to count).foldLeft(items) { (items, n) =>
                (1 until n).foldLeft(items) { (items, d) =>
                    if ((n % d) == 0) {
                        items.updated(d, items(d) + 1)
                    }
                    else {
                        items
                    }
                }
            }
            val et = System.nanoTime - st;
            println("vector," + count + "," + (et / 1000000))
        }
    }

    def runArray(counts: Array[Int]) {
        for (count <- counts) {
            val items = Array.fill(count)(0)
            val st = System.nanoTime
            val counts = (1 to count).foldLeft(items) { (items, n) =>
                (1 until n).foldLeft(items) { (items, d) =>
                    if ((n % d) == 0) {
                        items(d) = items(d) + 1
                    }
                    items
                }
            }
            val et = System.nanoTime - st;
            println("array," + count + "," + (et / 1000000))
        }
    }

    def main(args: Array[String]): Unit = {

      val tests =  Array(100, 1000, 5000, 10000, 20000, 30000, 40000, 50000)

      for (x <- (1 until 5)) {
        runArray(tests)
        runVector(tests)
      }

    }

}
