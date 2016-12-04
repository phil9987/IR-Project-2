import scala.collection.mutable
import scala.collection.mutable.MutableList
import scala.io.Source
/**
  * Created by marc on 30/11/16.
  */
object QueryMetric {

  //P, R, F1, AP per query as well as MAP for the entire system

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
        val title = line.replaceAll("<title>\\s*Topic:\\s*", "").trim
        codeToQuery(i) = title
      }
    }

    logger.log("reading relevance judgements")
    for (line <- Source.fromFile("./src/main/resources/relevance-judgements.csv").getLines()) {
      val tokens = line.split(' ')
      val id = Integer.parseInt(tokens(0))
      val docName = tokens(2).replaceAllLiterally("-", "")
      codeToJudgement(id)  = codeToJudgement.getOrElse(id, new MutableList[(String, Int)]) :+ ((docName,
        Integer
        .parseInt
        (tokens(3)) ))
    }
  }
  init()


  def eval(queryId: Int, retrievedDocs: List[String]) : (mutable.HashMap[Int, Double], mutable.HashMap[Int, Double],
    mutable.HashMap[Int, Double], Double) =
  {
    val relevantDocs = codeToJudgement(queryId).filter(_._2 == 1).map(_._1)
    val irrelevantDocs = codeToJudgement(queryId).filter(_._2 == 0).map(_._1)
    val retrievedIrrelevant = irrelevantDocs.intersect(retrievedDocs)
    if (retrievedIrrelevant.length > 0)
      logger.log("WARNING: retrieved documents that were explicitly marked as irrelevant in qrls: " +
                   retrievedIrrelevant
        .mkString("[", ", ", "]") )

    val precisionAtRank = new mutable.HashMap[Int, Double]
    val recallAtRank = new mutable.HashMap[Int, Double]
    val F1AtRank = new mutable.HashMap[Int, Double]


    for( k <- 1 to 100){
      val retrievedDocsAtK = retrievedDocs.take(k)
      val TP = relevantDocs.intersect(retrievedDocsAtK)
      val FP = retrievedDocsAtK.filter( !TP.contains(_) )
      val FN = relevantDocs.filter( !TP.contains(_) )
      precisionAtRank(k) = TP.length / (TP.length + FP.length + Double.MinPositiveValue)
      recallAtRank(k) = TP.length / (TP.length + FN.length)
      F1AtRank(k) = 2*precisionAtRank(k)*recallAtRank(k)/(precisionAtRank(k)+recallAtRank(k))
    }

    val TP = relevantDocs.intersect(retrievedDocs)
    val FN = relevantDocs.filter( !TP.contains(_) )

    val AP = retrievedDocs.map(  x => if (relevantDocs.contains(x)) 1.0 else 0.0 ).zipWithIndex.map(x => (x._1,
      precisionAtRank(x._2+1) ) ).map(x => x._1 * x._2).sum / (TP.length + FN.length)

    (precisionAtRank, recallAtRank, F1AtRank, AP)
  }


}
