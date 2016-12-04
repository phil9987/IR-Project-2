/**
  * Created by marc on 29/11/16.
  */
abstract class RankingModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor) {


  /**
    * Queries the model for a given list of words.
    * Implementations of this should:
    * - Use the InvertedIndex's dictionary to convert the query into a vector.
    * - Load relevant words, and their documents from the inverted index.
    * - Perform calculations based on the model.
    * - Rank the documents according
    * - Return the list of ranked documentIds
    * @param query List of query words.
    * @return A ranked List of documentIds.
    */
  //def query(query: List[String]): List[Int]

}

/**
  * This is a dummy ranking model. It shows how to imeplement other ranking models. Do not use.
  * It preprocesses words same as the reader did, takes only words that are in the dictionary.
  * For the resulting words it looks up the relevant documents.
  * For each document it sums up the occurrences of all the query words and ranks by the sum.
  * @param invertedIndex Inveted Index used in the search.
  * @param preprocessor Preprocessor used in the search. Should be the same as used for creating the Reader/Index.
  */

class DefaultRankingModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor) extends RankingModel(invertedIndex, preprocessor)
{
  val logger = new Logger("DefaultRankingModel")

  def query(query: List[String]): List[Int] = {

    //preprocess words
    logger.log("Querying with: " + query.mkString("[", ", ", "]"))
    val words = preprocessor.preprocess(query).distinct.toSet.intersect(invertedIndex.dictionary.keySet)
    logger.log("Using words: " + words.mkString("[", ", ", "]"))


    //load relevant docs
    val docs = words.flatMap( invertedIndex.invertedIndex(_) )
    //for all the found docs sum over the number of word occurrences for all words
    // and rank by the number of words
    val rankedDocs = docs.groupBy(_.docNb).mapValues( x => x.map(_.numOccurrence).sum ).toList.sortBy(- _._2 ).take(100)
    rankedDocs.map(_._1)
  }
}

class LanguageModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor, r : DocumentReader) extends RankingModel(invertedIndex, preprocessor)
{
  val logger = new Logger("LanuageModel")

  def scoringFunction(infoList : List[ExtendedWordInfo]): Double = {
    val lambda = 0.8
    val docLength = r.idToDocinfos(infoList(0).docNb).numWords
    infoList.map(
        x => lambda * x.numOccurrence / docLength + (1-lambda) * (r.wordCounts(x.word).frequencyCount )/ r.totalNumberOfWords
    ).product
  }

  def query(query: List[String], queryId : Int = -1 ): List[Int] = {

    //preprocess words
    logger.log("Querying with: " + query.mkString("[", ", ", "]"))
    val words = preprocessor.preprocess(query).distinct.toSet.intersect(invertedIndex.dictionary.keySet).toList.sorted
    logger.log("Using words: " + words.mkString("[", ", ", "]"))

    val tfMap = invertedIndex.naiveIntersect(words)
    logger.log(s"total documents returned : ${tfMap.size}")
    tfMap.foreach(x=> assert(x._2.length == words.size)) //TODO : remove this  at the end

    val scoresPerDoc = tfMap.mapValues(scoringFunction(_))
    val rankedDocs = scoresPerDoc.toList.sortBy( - _._2 ).take(100).map(_._1)

    logger.log("TOP 100 DOCS : ")
    var i = 0
    for (doc <- rankedDocs) {
      i+= 1
      var color = Console.RESET
      if (queryId != -1) {
        var judgement = QueryMetric.codeToJudgement(queryId).filter(x => x._1 == r.idToDocinfos(doc).docName)
        if (judgement.length == 0) {
          color = Console.RED
        } else if (judgement(0)._2 == 1) {
          color = Console.GREEN
        }
        else {
          color = Console.RED
        }
      }
      logger.log(s"$color TOP $i : ${r.idToDocinfos(doc).docName} (${r.idToDocinfos(doc).numWords} tokens) => ${tfMap(doc).sortBy(_.word).map(x => s"${x.word}: ${x.numOccurrence}").mkString(",")}")
      print(Console.RESET)
    }
    rankedDocs
  }
}


class VectorSpaceModel(invertedIndex : InvertedIndex, preprocessor : WordPreprocessor, r: DocumentReader) extends RankingModel(invertedIndex, preprocessor)
{
  val logger = new Logger("VectorSpaceModel")

  def normalize(v : List[Double]): List[Double] = {
    val divisor = math.sqrt(v.map(x=> x * x).sum)
    v.map(  _ / divisor)
  }

  def multiply(x : List[Double], y : List[Double]): Double = {
    assert(x.length == y.length)
    (normalize(x) zip normalize(y)).map( x=> x._1 * x._2 ).sum
  }


  def scoringFunction(infoList : List[ExtendedWordInfo], query : List[String]): Double = {
    val docVector = infoList.sortBy(_.word).map(x => x.numOccurrence * math.log(r.numOfDocs / r.wordCounts(x.word).docCount))
    val queryVector = query.sorted.map(word => 1.0 )
    multiply(docVector, queryVector)
  }

  def query(query: List[String], queryId : Int = -1 ): List[Int] = {
    //preprocess words
    logger.log("Querying with: " + query.mkString("[", ", ", "]"))
    val words = preprocessor.preprocess(query).distinct.toSet.intersect(invertedIndex.dictionary.keySet).toList.sorted
    logger.log("Using words: " + words.mkString("[", ", ", "]"))

    val tfMap = invertedIndex.naiveIntersect(words)
    logger.log(s"total documents returned : ${tfMap.size}")
    tfMap.foreach(x=> assert(x._2.length == words.size)) //TODO : remove this  at the end

    val scoresPerDoc = tfMap.mapValues(scoringFunction(_, words))
    val rankedDocs = scoresPerDoc.toList.sortBy( - _._2 ).take(100).map(_._1)


    logger.log("TOP 100 DOCS : ")
    var i = 0
    for (doc <- rankedDocs) {
      i+= 1

      var color = Console.RESET
      if (queryId != -1) {
        var judgement = QueryMetric.codeToJudgement(queryId).filter(x => x._1 == r.idToDocinfos(doc).docName)
        if (judgement.length == 0) {
          color = Console.RED
        } else if (judgement(0)._2 == 1) {
          color = Console.GREEN
        }
        else {
          color = Console.RED
        }
      }
      logger.log(s"$color TOP $i : ${r.idToDocinfos(doc).docName} (${r.idToDocinfos(doc).numWords} tokens) => ${tfMap(doc).sortBy(_.word).map(x => s"${x.word}: ${x.numOccurrence}").mkString(",")}")
      print(Console.RESET)
    }
    rankedDocs
  }
}