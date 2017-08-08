import scala.io._
import scala.collection._
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.immutable
import gnu.trove.decorator.TObjectIntMapDecorator
import gnu.trove.map.hash.TObjectIntHashMap

object WordCount {

  def toWords(lines: Array[String]): Array[String] = {
    lines
      .flatMap(line => line.split("\\s+"))
      .map(word => word.toLowerCase.replaceAll("[^a-z]", ""))
      .filter(word => !word.isEmpty)
  }

  def countWordsMutable(words: Seq[String]): Map[String, Int] = {
    val counts = new mutable.HashMap[String, Int]()
    for (word <- words) {
      counts(word) = counts.getOrElse(word, 0)
    }
    counts
  }

  def countWordsMutableJava(words: Seq[String])
    : java.util.HashMap[String, Int] =
  {
    val counts = new java.util.HashMap[String, Int]()
    for (word <- words) {
      counts.put(word, counts.getOrDefault(word, 0) + 1)
    }
    counts
  }

  def countWordsImmutable(words: Seq[String]): Map[String, Int] = {
    words.foldLeft(new immutable.HashMap[String, Int]) { (counts, word) =>
      counts.updated(word, counts.getOrElse(word, 0) + 1)
    }
  }

  def countWordsPrimitiveMap(words: Seq[String]): TObjectIntHashMap[String] = {
    val counts = new TObjectIntHashMap[String]()
    for (word <- words) {
      counts.put(word, if (counts.containsKey(word)) counts.get(word) + 1 else 1)
    }
    counts
  }

  def countWordsTree(words: Seq[String]): Map[String, Int] = {
    words.foldLeft(new immutable.TreeMap[String, Int]) { (counts, word) =>
      counts.updated(word, counts.getOrElse(word, 0))
    }
  }

  def runTest[T](name: String, words: Seq[String], counter: Seq[String] => T) = {
    val st = System.nanoTime()
    val counts = counter(words)
    val et = System.nanoTime() - st
    println(name + "," + (et / 1000000.0) + "," + words.size)
    et
  }

  def main(args: Array[String]): Unit = {
    val content = toWords(Source.fromFile("pg100.txt", "UTF-8").getLines().toArray)
    val words = Iterator.continually(content).flatten.take(30000000).toArray

    runTest("immutable", words.take(5000), countWordsImmutable)
    runTest("mutableJava", words.take(5000), countWordsMutableJava)

    println("name,time,count")

    var x = 10000.0
    while (x < 30000000) {
      runTest("immutable", words.take(x.toInt), countWordsImmutable)
      x = x * 1.05
    }
    x = 10000.0
    while (x < 30000000) {
      runTest("mutableJava", words.take(x.toInt), countWordsMutableJava)
      x = x * 1.05
    }
  }

}
