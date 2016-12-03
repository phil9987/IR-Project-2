/**
  * Created by marc on 29/11/16.
  */
abstract class InvertedIndex(documentReader: DocumentReader) {
  val logger = new Logger("InvertedIndex")
  /**
    * Maps a word to the position in the dictionary.
    */
  val dictionary : Map[String, Int] = documentReader.dictionary

  /**
    * Provides a list of DocInfos for a given word (inverted Index).
    */
  val invertedIndex : Map[String, List[WordInfo]] = documentReader.postings.toMap

  /**
    * given a list of query term, returns a map from docNb to the List of ExtendedInfo's
    */
  def naiveIntersect ( queryTerms : List[String]): Map[Int, List[ExtendedWordInfo]] =  {
      logger.log("number of documents containing each queryterm : ")
      queryTerms.map(q=> (q, invertedIndex(q))).foreach( x=> logger.log(s" - ${x._1} : ${x._2.length} docs ") )
      def extend(infoList : List[ExtendedWordInfo]) : List[ExtendedWordInfo] =  {
        var newList = infoList
        queryTerms.foreach{
        case (term )=> {
            if (!newList.exists{ x=> x.word == term}){
              newList = newList :+ ExtendedWordInfo(term, newList(0).docNb, 0, false)
            }
          }
        }
        newList.sortBy(_.word)
      }

      queryTerms.flatMap( q => invertedIndex(q).map(wi => ExtendedWordInfo(q, wi.docNb, wi.numOccurrence, wi.isInHeader))).
      groupBy(_.docNb).mapValues(extend(_))
  }
}

class DefaultInvertedIndex(documentReader: DocumentReader) extends InvertedIndex(documentReader)
{
}