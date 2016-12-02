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
  def query(query: List[String]): List[Int]

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
    println(docs.toString)
    //for all the found docs sum over the number of word occurrences for all words
    // and rank by the number of words
    val rankedDocs = docs.groupBy(_.docNb).mapValues( x => x.map(_.numOccurrence).sum ).toList.sortBy(- _._2 ).take(100)
    rankedDocs.map(_._1)
  }
}

class LanguageModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor, reader : DocumentReader) extends RankingModel(invertedIndex, preprocessor)
{
  val logger = new Logger("LanuageModel")

  def scoringFunction(infoList : List[ExtendedWordInfo]): Double = {
    val lambda = 0.8
    val document = infoList(0).docNb
    val docLength = reader.idToDocinfos(document).numWords
    //TODO : Replace with real total word counts
    infoList.map( x => lambda * x.numOccurrence / docLength + (1-lambda) * (reader.wordCounts(x.word).frequencyCount )/ 1000000 ).product
  }

  def query(query: List[String]): List[Int] = {

    //preprocess words
    logger.log("Querying with: " + query.mkString("[", ", ", "]"))
    val words = preprocessor.preprocess(query).distinct.toSet.intersect(invertedIndex.dictionary.keySet)
    logger.log("Using words: " + words.mkString("[", ", ", "]"))

    val tfMap = invertedIndex.naiveIntersect(words.toList)
    println(tfMap.take(10))
    val scoresPerDoc = tfMap.mapValues(scoringFunction(_))
    val rankedDocs = scoresPerDoc.toList.sortBy( - _._2 ).take(10)
    println(rankedDocs.map( x=> (reader.idToDocinfos(x._1).docName, x._1, x._2)))
    rankedDocs.map(_._1)

  }
}
