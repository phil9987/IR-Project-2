/**
  * Created by marc on 29/11/16.
  */
abstract class RankingModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor, r: DocumentReader){
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
  val logger = new Logger("AbstractRankingModel")

  def setHyperParameters(theta : Double, zeta : Int, fh : Double) : Unit= {}
  def setHyperParameters(fh : Double) : Unit= {}
  def setModelMode (mode : String): Unit = {}
  //def query(query: List[String]): List[Int]
  def scoringFunction(infoList : List[ExtendedWordInfo], query : List[String]): Double = {
    return 1.0
  }


  def query(query: List[String], queryId : Int = -1, verbose : Boolean = false): List[Int] = {
    //preprocess words
    //logger.log("Querying with: " + query.mkString("[", ", ", "]"))
    val words = preprocessor.preprocess(query).distinct.toSet.intersect(invertedIndex.dictionary.keySet).toList.sorted
    //logger.log("Using words: " + words.mkString("[", ", ", "]"))

    val tfMap = invertedIndex.naiveIntersect(words)
    if (verbose) logger.log(s"total documents returned : ${tfMap.size}")
    tfMap.foreach(x=> assert(x._2.length == words.size)) //TODO : remove this  at the end

    val scoresPerDoc = tfMap.mapValues(scoringFunction(_, words))
    val rankedDocs = scoresPerDoc.toList.sortBy( - _._2 ).take(100).map(_._1)

    if (verbose) {
      logger.log("TOP 30 DOCS : ")
      var i = 0
      for (doc <- rankedDocs.take(30)) {
        i += 1
        var color = Console.RESET
        if (queryId != -1) {
          var judgement = QueryMetric.codeToJudgement(queryId).filter(x => x._1 == r.idToDocinfos(doc).docName)
          if (judgement.length == 0) {
            color = Console.YELLOW
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
    }
    rankedDocs
  }

}

/**
  * This is a dummy ranking model. It shows how to imeplement other ranking models. Do not use.
  * It preprocesses words same as the reader did, takes only words that are in the dictionary.
  * For the resulting words it looks up the relevant documents.
  * For each document it sums up the occurrences of all the query words and ranks by the sum.
  * @param invertedIndex Inveted Index used in the search.
  * @param preprocessor Preprocessor used in the search. Should be the same as used for creating the Reader/Index.
  */

class DefaultRankingModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor, r : DocumentReader) extends RankingModel(invertedIndex, preprocessor, r)
{
  override val logger = new Logger("DefaultRankingModel")
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

class LanguageModel(invertedIndex: InvertedIndex, preprocessor: WordPreprocessor, r : DocumentReader) extends RankingModel(invertedIndex, preprocessor, r)
{

  def toDouble(b : Boolean): Double ={
    if (b) return 1.0
    else return 0.0
  }

  override val logger = new Logger("LanuageModel")

  var lambda : Double = 0.8
  var zeta : Double = 100
  var fancyHitBonus : Double = 0.0


  override def setHyperParameters(newLambda : Double, newZeta : Int, newFancyHitBonus : Double): Unit ={
    lambda = newLambda;
    zeta = newZeta;
    fancyHitBonus = newFancyHitBonus;
  }

  override def scoringFunction(infoList : List[ExtendedWordInfo], query : List[String]): Double = {
    val docLength = r.idToDocinfos(infoList(0).docNb).numWords
    infoList.map(
        x => math.log(lambda * ( x.numOccurrence + fancyHitBonus * toDouble(x.isInHeader)) / (docLength + zeta) + (1-lambda) * (r.wordCounts(x.word).frequencyCount )/ r.totalNumberOfWords)
    ).sum
  }
}

class VectorSpaceModel(invertedIndex : InvertedIndex, preprocessor : WordPreprocessor, r: DocumentReader) extends RankingModel(invertedIndex, preprocessor, r)
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

  def tf(info : ExtendedWordInfo, isDocument : Boolean) : Double = {
    var occurrence = if(isDocument && info.isInHeader) info.numOccurrence + fancyHitBonus else info.numOccurrence
    var tfMode =  if (isDocument) modelMode(0) else modelMode(4)
    assert(tfMode == 'n' || tfMode == 'l' || tfMode == 'b')
    if(tfMode == 'n')
      return occurrence
    else if (tfMode == 'l'){
      if (occurrence == 0)
        return 0
      else
        return 1 + math.log(occurrence)
    }
    else {
      assert(tfMode == 'b')
      if ( occurrence == 0)
        return 0
      else
        return 1
    }
  }

  def idf(info : ExtendedWordInfo, isDocument : Boolean) : Double = {
    var idfMode = if (isDocument) modelMode(1) else modelMode(5)
    assert(idfMode == 'n' || idfMode == 't' || idfMode == 'p')
    if (idfMode == 'n')
      return 1
    else if (idfMode == 't'){
      return math.log(r.numOfDocs / r.wordCounts(info.word).docCount)
    }
    else {
      assert(idfMode == 'p')
        return math.max(0, math.log( (r.numOfDocs - r.wordCounts(info.word).docCount) / r.wordCounts(info.word).docCount))
    }
  }

  def dotProduct(doc : List[Double], query : List[Double]): Double = {
    assert(doc.length == query.length)
    (doc zip query).map(x => x._1 * x._2).sum
  }

  def normalize(v : List[Double], docId : Int,  isDocument : Boolean): List[Double] = {
    var normMode = if (isDocument) modelMode(2)  else modelMode(6)
    assert(normMode == 'n' || normMode == 'c')
    if (normMode == 'n' )
      return v
    else {
      assert(normMode == 'c')
      val divisor = if (docId == -1) math.sqrt(v.map(x => x * x).sum) else (r.idToDocinfos(docId).vectorLength)
      return v.map(_ / divisor)
    }
  }

  override def scoringFunction(infoList : List[ExtendedWordInfo], query : List[String]): Double = {
    val docVector = infoList.sortBy(_.word).map(info => tf(info, true) * idf(info, true))
    val queryVector = query.sorted.map(word => ExtendedWordInfo(word, infoList(0).docNb, 1, false)).map(
      info => tf(info, false)* idf(info, false))
    dotProduct(normalize(docVector, infoList(0).docNb, true), normalize(queryVector, -1,  false))
  }
}
