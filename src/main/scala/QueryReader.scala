import scala.collection.mutable
import scala.io.Source


/**
  * Reads the test queries and provides them to be used.
  */
object QueryReader {

  /**
    * Contains the queries. Mapping id -> query.
    */
  val codeToQuery = new mutable.HashMap[Int, String]

  //parse query titles (=queries) and queryIds from the input document
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
