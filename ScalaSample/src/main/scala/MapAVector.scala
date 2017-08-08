import scala.collection.Seq
import scala.collection.immutable._
import scala.collection.mutable.ArrayBuffer


object MapAVector {

  def mapByPure(xs: Vector[Int])(f: Int => Int): Vector[Int] = {
    (0 until xs.length).foldLeft(xs) { (xs, i) => xs.updated(i, f(xs(i))) }
  }

  def mapByNative(xs: Vector[Int])(f: Int => Int): Vector[Int] = {
    xs.map(f)
  }

  def mapByImpure(xs: Vector[Int])(f: Int => Int): Vector[Int] = {
    val a = xs.toArray
    for (i <- 0 until a.length) {
      a(i) = f(a(i))
    }
    a.toVector
  }

  def mapByBuilder(xs: Vector[Int])(f: Int => Int): Vector[Int] = {
    val b = xs.genericBuilder[Int]
    b.sizeHint(xs.size)
    for (i <- 0 until xs.length) {
      b += f(xs(i))
    }
    b.result()
  }

  def runTest(name: String, numbers: Vector[Int], mutator: Vector[Int] => Vector[Int], times: Int) = {
    val rts = new ArrayBuffer[Double]()
    for (time <- 0 until times) {
      val st = System.nanoTime()
      mutator(numbers)
      val et = System.nanoTime() - st
      rts.append(et)
      println(name + "," + (et / 1000000.0) + "," + numbers.size)
    }
    println(name + ",median = " + rts.sorted.apply(rts.length / 2) / 1000000.0)
  }

  def main(args: Array[String]): Unit = {
    val numbers = Vector.fill(1000000)(0)
    runTest(
      "pure",
      numbers,
      { numbers => mapByPure(numbers) { x => x + 1 } },
      20)
    runTest(
      "mutation",
      numbers,
      { numbers => mapByImpure(numbers) { x => x + 1 } },
      20)
    runTest(
      "native",
      numbers,
      { numbers => mapByNative(numbers) { x => x + 1 } },
      20)

    runTest(
      "builder",
      numbers,
      { numbers => mapByBuilder(numbers) { x => x + 1 } },
      20)
  }


}
