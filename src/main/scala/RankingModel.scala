/**
  * Created by marc on 29/11/16.
  */

//TODO comment me
case class QueryResult(rankedDocs: List[String], docToWordMap : Map[String, List[WordInDocInfo]])

/**
  * The RankingModel implements the core of the search engine. It retrieves the relevant documents form the inverted
  * index and ranks them according to some metric. This class is an abstract implementation and the scoringFunction
  * that does the actual ranking is not impelmented here. Classes deriving from this class need just to
  * reimplement the scoringFunction in order to implement custom ranking algorithms.
  * @param invertedIndex The inverted index, from which to load data.
  * @param preprocessor WordPreprocessor that will be applied to the queries. Should be the same that was applied to
  *                     the documents when reading from them.
  */
//TODO remove r
abstract class RankingModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor){
  val logger = new Logger("AbstractRankingModel")

  //TODO move to parameters
  def setHyperParameters(theta : Double, zeta : Int, fh : Double) : Unit= {}
  def setHyperParameters(fh : Double) : Unit= {}
  def setModelMode (mode : String): Unit = {}

  /**
    * Scores documents for a given Query. This determines the ranking.
    * Abstract Implementation. Derived classes should implement a mathematical RankingModel here.
    * @param infoList List of WordInDocInfo loaded for the query and a given document.
    * @param query List of the query words.
    * @return Score for the document.
    */
  def scoringFunction(infoList : Iterable[WordInDocInfo], query : List[String]): Double

  /**
    * Given a list of WordInDocInfo for one document and the list of query terms for one document, extends the list
    * to also include WordInDocInfo (with nrOccurrence=0) for the words not occurring in the document.
    * @param infoList The given list of WordInDocInfo for one document.
    * @param queryTerms The list of query terms for a query.
    * @return The infoList extended to also include WordInDocInfos for queryTerms that did not occur in the document.
    */
  def extend(infoList : List[WordInDocInfo], queryTerms : List[String]) : List[WordInDocInfo] = {
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

  /**
    * Given a preprocessed query (words) and a docName to WordInDocInfos Map performs the actual ranking and returns
    * the result.
    * @param docToWordMap A map containing a key for all relevant documents and for each key a list of WordInDocInfo
    *                     for all query terms.
    * @param words The processed query terms.
    * @return A QueryResult containing the ranked documents and the docToWordMap.
    */
  def query(docToWordMap : Map[String, List[WordInDocInfo]], words : List[String]) : QueryResult = {
    val scoresPerDoc = docToWordMap.mapValues(scoringFunction(_, words))
    val rankedDocs = scoresPerDoc.toList.sortBy( - _._2 ).take(100).map(_._1)
    QueryResult(rankedDocs, docToWordMap)
  }


  /**
    * Given a query, preprocess it load the WordInDocInfos and call query(docToWordMap, words) where words are the
    * preprocessed query.
    * This funktion returns after the query and presents a result.
    * @param queryWords The query words.
    * @return A QueryResult.
    */
  def query(queryWords: List[String]): QueryResult = {
    //preprocess the query
    //logger.log("Querying with: " + query.mkString("[", ", ", "]"))
    val words = preprocessor.preprocess(queryWords).distinct.toSet.intersect(invertedIndex.dictionary.keySet).toList.sorted
    logger.log("Using words: " + words.mkString("[", ", ", "]"))

    //load information of word occurrences form inverted index
    val docToWordMap = invertedIndex.getDocsForWords(words).groupBy(_.docName).mapValues(w => extend(w.toList, words))
    logger.log(s"total documents returned : ${docToWordMap.size}")
    docToWordMap.foreach(x=> assert(x._2.size == words.size)) //TODO : remove this  at the end

    //handle ranking and return result
    query(docToWordMap, words)
  }

  //We can also perform multiple queries in batch mode. This is relevant if not using an inverted index.

  /**
    * Stores the Queries in batch-mode.
    */
  protected val batchQueryQueue = new scala.collection.mutable.MutableList[List[String]]

  /**
    * Add a query to the list of queries to execute in batch mode. Returns after enqueuing. No result provided.
    * @param query The query.
    */
  def enqueueBatchQuery(query: List[String]) : Unit = {
    batchQueryQueue += query
  }

  /**
    * Performs all currently queued queries for the batch mode. Only does one query to the inverted index, which is
    * important when using a pass-through index (no inverted index data structure and full data scan).
    * @return List of QueryResults in the order they were enqueued.
    */
  def performBatchQuery() : List[QueryResult] = {
    //the the query terms for all queries currently queued
    val queryWords = batchQueryQueue.map( preprocessor.preprocess(_).distinct.toSet.intersect(invertedIndex.dictionary
                                                                                                .keySet).toList
                                            .sorted )
    //clear the list of queued queries
    batchQueryQueue.clear()

    //one pass through all documents
    val invertedIndexQueryResults = invertedIndex.getDocsForWords(queryWords.flatten.distinct).groupBy(_.word)

    //Create the docToWordMap for all queries
    val WordAndDocToWordMapResults = queryWords.map( words => (words, words.flatMap(invertedIndexQueryResults(_))
      .groupBy(_ .docName).mapValues(w => extend(w.toList, words))) )

    //Perform the ranking for all queries
    WordAndDocToWordMapResults.map( x => query(x._2, x._1) ).toList
  }

}


class LanguageModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor) extends RankingModel(invertedIndex, preprocessor)
{

  def toDouble(b : Boolean): Double ={
    if (b) 1.0
    else 0.0
  }

  override val logger = new Logger("LanuageModel")

  var lambda : Double = 0.8
  var zeta : Double = 100
  var fancyHitBonus : Double = 0.0


  override def setHyperParameters(newLambda : Double, newZeta : Int, newFancyHitBonus : Double): Unit ={
    lambda = newLambda
    zeta = newZeta
    fancyHitBonus = newFancyHitBonus
  }

  override def scoringFunction(infoList : Iterable[WordInDocInfo], query : List[String]): Double = {
    val docLength = invertedIndex.getDocLength(infoList.head.docId)
    infoList.map(
        x => math.log(lambda * ( x.numOccurrence + fancyHitBonus * toDouble(x.isInHeader)) / (docLength + zeta) +
                        (1-lambda) * invertedIndex.getWordCount(x.word).frequencyCount/ invertedIndex.getTotalNumberOfWords)
    ).sum
  }
}

class VectorSpaceModel(invertedIndex : InvertedIndex, preprocessor : WordPreprocessor) extends RankingModel(invertedIndex, preprocessor)
{

  /**
  the mode for the model
  first three letters : How to represent the document vector
  last three letters : How to represent the query vector
  a 3-letter block defines the following methods :
      first letter : how to count the term frequency (can be n for natural, l for logarithm, b for boolean)
      second letter : how to count the idf ( can b n for none or t for log(N/df))
      third letter : how to normalize the vector ( n for none, c for cosine )
   */
  var modelMode : String = "nnn.nnn"
  var fancyHitBonus = 0.0

  override def setModelMode (mode : String): Unit = {
    modelMode = mode
  }

  override def setHyperParameters(fhb : Double): Unit ={
    fancyHitBonus = fhb
  }
  override val logger = new Logger("VectorSpaceModel")

  def tf(info : WordInDocInfo, isDocument : Boolean) : Double = {
    var occurrence = if(isDocument && info.isInHeader) info.numOccurrence + fancyHitBonus else info.numOccurrence
    var tfMode =  if (isDocument) modelMode(0) else modelMode(4)
    assert(tfMode == 'n' || tfMode == 'l' || tfMode == 'b')
    if(tfMode == 'n')
      occurrence
    else if (tfMode == 'l'){
      if (occurrence == 0)
        0
      else
        1 + math.log(occurrence)
    }
    else {
      assert(tfMode == 'b')
      if ( occurrence == 0)
        0
      else
        1
    }
  }

  def idf(info : WordInDocInfo, isDocument : Boolean) : Double = {
    val idfMode = if (isDocument) modelMode(1) else modelMode(5)
    assert(idfMode == 'n' || idfMode == 't' || idfMode == 'p')
    if (idfMode == 'n')
      1
    else if (idfMode == 't'){
      math.log(invertedIndex.getDocCount / invertedIndex.getWordCount(info.word).docCount)
    }
    else {
      assert(idfMode == 'p')
        math.max(0, math.log( (invertedIndex.getDocCount - invertedIndex.getWordCount(info.word).docCount) / invertedIndex.getWordCount(info.word).docCount))
    }
  }

  def dotProduct(doc : List[Double], query : List[Double]): Double = {
    assert(doc.length == query.length)
    (doc zip query).map(x => x._1 * x._2).sum
  }

  def normalize(v : List[Double], isDocument : Boolean): List[Double] = {
    var normMode = if (isDocument) modelMode(2)  else modelMode(6)
    assert(normMode == 'n' || normMode == 'c')
    if (normMode == 'n' )
      v
    else {
      assert(normMode == 'c')
      val divisor = math.sqrt(v.map(x => x * x).sum)
      v.map(_ / divisor)
    }
  }
//TODO singature to list
  override def scoringFunction(infoList : Iterable[WordInDocInfo], query : List[String]): Double = {
    val docVector = infoList.toList.sortBy(_.word).map(info => tf(info, true) * idf(info, true))
    val queryVector = query.sorted.map(word => WordInDocInfo(word, infoList.toList.head.docName, infoList.toList
      .head.docId, 1, false)
    ).map(
      info => tf(info, false)* idf(info, false))
    dotProduct(normalize(docVector, true), normalize(queryVector, false))
  }
}
