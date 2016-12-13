/**
  * The RankingModel implements the core of the search engine. It retrieves the relevant documents form the inverted
  * index and ranks them according to some metric. This class is an abstract implementation and the scoringFunction
  * that does the actual ranking is not implemented here. Classes deriving from this class need just to
  * reimplement the scoringFunction in order to implement custom ranking algorithms.
  *
  * @param invertedIndex The inverted index, from which to load data.
  * @param preprocessor  WordPreprocessor that will be applied to the queries. Should be the same that was applied to
  *                      the documents when reading from them.
  */
abstract class RankingModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor) {
  val ii = invertedIndex
  val logger = new Logger("AbstractRankingModel")
  /**
    * Stores the Queries in batch-mode.
    */
  protected val batchQueryQueue = new scala.collection.mutable.MutableList[List[String]]

  /**
    * Scores documents for a given Query. This determines the ranking.
    * Abstract Implementation. Derived classes should implement a mathematical RankingModel here.
    *
    * @param infoList List of WordInDocInfo loaded for the query and a given document.
    * @param query    List of the query words.
    * @return Score for the document.
    */
  def scoringFunction(infoList: List[WordInDocInfo], query: List[String]): Double

  /**
    * Given a query, preprocess it load the WordInDocInfos and call query(docToWordMap, words) where words are the
    * preprocessed query.
    * This function returns after the query and presents a result.
    *
    * @param queryWords The query words.
    * @return A QueryResult.
    */
  def query(queryWords: List[String]): QueryResult = {
    //preprocess the query
    //logger.log("Querying with: " + query.mkString("[", ", ", "]"))
    val words = preprocessor.preprocess(queryWords).distinct.toSet.intersect(invertedIndex.dictionary.keySet).toList
      .sorted
    logger.log("Using words: " + words.mkString("[", ", ", "]"))

    //load information of word occurrences form inverted index
    val docToWordMap = invertedIndex.getDocsForWords(words).groupBy(_.docName).mapValues(w => extend(w, words))
    logger.log(s"total documents returned : ${docToWordMap.size}")
    //    docToWordMap.foreach(x=> assert(x._2.size == words.size))

    //handle ranking and return result
    query(docToWordMap, words)
  }

  /**
    * Given a list of WordInDocInfo for one document and the list of query terms for one document, extends the list
    * to also include WordInDocInfo (with nrOccurrence=0) for the words not occurring in the document.
    *
    * @param infoList   The given list of WordInDocInfo for one document.
    * @param queryTerms The list of query terms for a query.
    * @return The infoList extended to also include WordInDocInfos for queryTerms that did not occur in the document.
    */
  def extend(infoList: List[WordInDocInfo], queryTerms: List[String]): List[WordInDocInfo] = {
    var newList = infoList
    queryTerms.foreach {
                         case (term) =>
                           if (!newList.exists { x => x.word == term }) {
                             newList = newList :+ WordInDocInfo(term, newList.head.docName, newList.head.docId, 0,
                                                                isInHeader = false)
                           }
                       }
    newList.sortBy(_.word)
  }

  //We can also perform multiple queries in batch mode. This is relevant if not using an inverted index.

  /**
    * Given a preprocessed query (words) and a docName to WordInDocInfos Map performs the actual ranking and returns
    * the result.
    *
    * @param docToWordMap A map containing a key for all relevant documents and for each key a list of WordInDocInfo
    *                     for all query terms.
    * @param words        The processed query terms.
    * @return A QueryResult containing the ranked documents and the docToWordMap.
    */
  def query(docToWordMap: Map[String, List[WordInDocInfo]], words: List[String]): QueryResult = {
    val scoresPerDoc = docToWordMap.mapValues(scoringFunction(_, words))
    val rankedDocs = scoresPerDoc.toList.sortBy(-_._2).take(100).map(_._1)
    QueryResult(rankedDocs, docToWordMap)
  }

  /**
    * Add a query to the list of queries to execute in batch mode. Returns after enqueuing. No result provided.
    *
    * @param query The query.
    */
  def enqueueBatchQuery(query: List[String]): Unit = {
    batchQueryQueue += query
  }

  /**
    * Performs all currently queued queries for the batch mode. Only does one query to the inverted index, which is
    * important when using a pass-through index (no inverted index data structure and full data scan).
    *
    * @return List of QueryResults in the order they were enqueued.
    */
  def performBatchQuery(): List[QueryResult] = {
    //the the query terms for all queries currently queued
    val queryWords = batchQueryQueue.map(preprocessor.preprocess(_).distinct.toSet.intersect(invertedIndex.dictionary
                                                                                               .keySet).toList
                                           .sorted)
    //clear the list of queued queries
    batchQueryQueue.clear()

    //one pass through all documents
    val invertedIndexQueryResults = invertedIndex.getDocsForWords(queryWords.flatten.distinct).groupBy(_.word)

    //Create the docToWordMap for all queries
    val WordAndDocToWordMapResults = queryWords.map(words => (words, words.flatMap(invertedIndexQueryResults(_))
      .groupBy(_.docName).mapValues(w => extend(w, words))))

    //Perform the ranking for all queries
    WordAndDocToWordMapResults.map(x => query(x._2, x._1)).toList
  }

}

/**
  * Holds the result of a query.
  *
  * @param rankedDocs   The ranked documents. Actual result of the query.
  * @param docToWordMap Additional result. Shows the details of which documents contain which words. Used for
  *                     additionall metrics and feedback.
  */
case class QueryResult(rankedDocs: List[String], docToWordMap: Map[String, List[WordInDocInfo]])

/**
  * Calculates the languages generated by each document, and then calculates the probability of the document language
  * generating the query
  *
  * @param invertedIndex The inverted index, from which to load data.
  * @param preprocessor  WordPreprocessor that will be applied to the queries. Should be the same that was applied to
  *                      the documents when reading from them.
  * @param lambda Double between 0 and 1, balances the document language with the collection language for smoothing
  * @param zeta Is added to every document length
  * @param fancyHitBonus Bonus occurence awarded if the word is present in the tile
  */
class LanguageModel(invertedIndex: InvertedIndex,
                    preprocessor: WordPreprocessor, lambda: Double, zeta: Int,
                    fancyHitBonus: Double) extends RankingModel(invertedIndex, preprocessor) {

  override val logger = new Logger("LanguageModel")

  override def scoringFunction(infoList: List[WordInDocInfo], query: List[String]): Double = {
    val docLength = invertedIndex.getDocLength(infoList.head.docId)
    infoList.map(
      x => math.log(lambda * (x.numOccurrence + fancyHitBonus * toDouble(x.isInHeader)) / (docLength + zeta) +
                      (1 - lambda) * invertedIndex.getWordCount(x.word).frequencyCount / invertedIndex
                        .getTotalNumberOfWords)
    ).sum
  }

  def toDouble(b: Boolean): Double = {
    if (b) 1.0
    else 0.0
  }
}

/**
  * Represents the documents and queries as vectors and calculates the similarity between them.
  *
  * @param invertedIndex The inverted index, from which to load data.
  * @param preprocessor  WordPreprocessor that will be applied to the queries. Should be the same that was applied to
  *                      the documents when reading from them.
  * @param modelMode SMART notation for the vector representation ( [nlb][ntp][nc].[nlb][ntp][nc] )
  * @param fancyHitBonus bonus occurence awarded if the word is present in the tile
  *
  * the mode for the model
  * first three letters : How to represent the document vector
  * last three letters : How to represent the query vector
  * a 3-letter block defines the following methods :
  * first letter : how to count the term frequency (can be n for natural, l for logarithm, b for boolean)
  * second letter : how to count the idf ( can be n for none or t for log(N/df))
  * third letter : how to normalize the vector ( n for none, c for cosine )
  */
class VectorSpaceModel(invertedIndex: InvertedIndex,
                       preprocessor: WordPreprocessor, modelMode: String,
                       fancyHitBonus: Double) extends RankingModel(invertedIndex,
                                                                   preprocessor) {

  override val logger = new Logger("VectorSpaceModel")

  override def scoringFunction(infoList: List[WordInDocInfo], query: List[String]): Double = {
      Vectors.score(infoList, query, modelMode, fancyHitBonus)
  }
}
