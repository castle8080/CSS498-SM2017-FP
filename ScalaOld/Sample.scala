import java.util.regex.Pattern

object Greeters {
  def create(format: String) = {
    val parts = format.split(Pattern.quote("#{name}"));
    (name: String) => parts.mkString(name)
  }
}

object Sample {
  def main(args: Array[String]) = {
    val nice = Greeters.create("Hi #{name}!"); 
    val mean = Greeters.create("Go away #{name}!"); 
    println(nice("Bob"));
    println(mean("Joe"));
  }
}

