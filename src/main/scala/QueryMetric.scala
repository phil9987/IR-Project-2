import scala.collection.mutable.{HashMap => MutHashMap, MutableList => MutList}
import scala.io.Source

/**
  * Reads the training queries, their relevant judgements (qrls) and provides them to be used.
  * Also provides methods to evaluate the results for those queries.
  */
object QueryMetric {
  private val logger = new Logger("QueryMetric")

  /**
    * Contains the queries. Mapping id -> query.
    */
  val codeToQuery = new MutHashMap[Int, String]

  /**
    * Contains the relevance judgments. Mapping id -> (docName, relevance).
    */
  val codeToJudgement = new MutHashMap[Int, MutList[(String, Int)]]

  //parse query titles (=queries) and queryIds from the input document
  logger.log("reading questions")
  var i: Int = -1
  for (line <- Source.fromFile("./src/main/resources/questions-descriptions.txt").getLines()) {
    if (line.startsWith("<num>"))
      i = Integer.parseInt(line.replaceAll("[^0-9]*", ""))
    if (line.startsWith("<title>")) {
      val title = line.replaceAll("<title>\\s*Topic:\\s*", "").trim
      codeToQuery(i) = title
    }
  }

  //parse relevance judgements
  logger.log("reading relevance judgements")
  for (line <- Source.fromFile("./src/main/resources/relevance-judgements.csv").getLines()) {
    val tokens = line.split(' ')
    val id = Integer.parseInt(tokens(0))
    val docName = tokens(2).replaceAllLiterally("-", "")
    codeToJudgement(id) = codeToJudgement.getOrElse(id, new MutList[(String, Int)]) :+ ((docName, Integer
      .parseInt(tokens(3))))
  }


  /**
    * Given a queryId and a ranking computes metrics (precession at rank 1-100, recall at rank 1-100, F1 at rank
    * 1-100, average precession).
    *
    * @param queryId       The queryId.
    * @param retrievedDocs The retrieved document names ordered by relevance.
    * @return (precession at rank 1-100, recall at rank 1-100, F1 at rank 1-100, average precession)
    */
  def eval(queryId: Int, retrievedDocs: List[String]): (MutHashMap[Int, Double], MutHashMap[Int, Double],
    MutHashMap[Int, Double], Double) = {
    val relevantDocs = codeToJudgement(queryId).filter(_._2 == 1).map(_._1)
    //    val irrelevantDocs = codeToJudgement(queryId).filter(_._2 == 0).map(_._1)
    //    val retrievedIrrelevant = irrelevantDocs.intersect(retrievedDocs)

    val precisionAtRank = new MutHashMap[Int, Double]
    val recallAtRank = new MutHashMap[Int, Double]
    val F1AtRank = new MutHashMap[Int, Double]

    //TODO adjust if there are less than 100 documents
    for (k <- 1 to 100) {
      val retrievedDocsAtK = retrievedDocs.take(k)
      val TP = relevantDocs.intersect(retrievedDocsAtK)
      val FP = retrievedDocsAtK.filter(!TP.contains(_))
      val FN = relevantDocs.filter(!TP.contains(_))
      //we add Double.MinPositiveValue in case the denominator is 0
      precisionAtRank(k) = 1.0 * TP.length / (TP.length + FP.length + Double.MinPositiveValue)
      recallAtRank(k) = 1.0 * TP.length / (TP.length + FN.length)
      F1AtRank(k) = 2 * precisionAtRank(k) * recallAtRank(k) / (precisionAtRank(k) + recallAtRank(k))
    }

    val TP = relevantDocs.intersect(retrievedDocs)
    val FN = relevantDocs.filter(!TP.contains(_))
    val AP = retrievedDocs.map(x => if (relevantDocs.contains(x)) 1.0 else 0.0).zipWithIndex.map(x => (x._1,
      precisionAtRank(x._2 + 1))).map(x => x._1 * x._2).sum / math.min(TP.length + FN.length, 100)

    (precisionAtRank, recallAtRank, F1AtRank, AP)
  }


}
