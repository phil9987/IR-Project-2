import scala.collection.mutable
import scala.collection.mutable.MutableList
import scala.io.Source

/**
  * Created by marc on 30/11/16.
  */
object QueryReader {
  val codeToQuery = new mutable.HashMap[Int, String]

  var i: Int = -1
  for (line <- Source.fromFile("./src/main/resources/test-questions.txt").getLines()) {
    if (line.startsWith("<num>"))
      i = Integer.parseInt(line.replaceAll("[^0-9]*", ""))
    if (line.startsWith("<title>")) {
      val title = line.replaceAll("<title>\\s*Topic:\\s*", "").trim
      codeToQuery(i) = title
    }
  }
}
