import scala.collection.mutable
import scala.collection.mutable.MutableList
import scala.io.Source
/**
  * Created by marc on 30/11/16.
  */
object QueryMetric {

  private val logger = new Logger("QueryMetric")
  val codeToQuery = new mutable.HashMap[Int, String]
  val codeToJudgement = new mutable.HashMap[Int, MutableList[(String, Int)]]

  def init() = {
    logger.log("reading questions")
    var i : Int = -1
    for (line <- Source.fromFile("./src/main/resources/questions-descriptions.txt").getLines()) {
      if (line.startsWith("<num>"))
        i = Integer.parseInt(line.replaceAll("[^0-9]*", ""))
      if (line.startsWith("<title>"))
      {
        val title = line.replace("<title> Topic: ", "").trim
        codeToQuery(i) = title
      }
    }

    logger.log("reading relevance judgements")
    for (line <- Source.fromFile("./src/main/resources/relevance-judgements.csv").getLines()) {
      val tokens = line.split(' ')
      val id = Integer.parseInt(tokens(0))
      codeToJudgement(id)  = codeToJudgement.getOrElse(id, new MutableList[(String, Int)]) :+ ((tokens(2), Integer
        .parseInt
        (tokens(3)) ))
    }
  }
  init()

}
