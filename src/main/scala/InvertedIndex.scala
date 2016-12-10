import ch.ethz.dal.tinyir.processing.Tokenizer

/**
  * Created by marc on 29/11/16.
  */


/**
  * The inverted Index used for Document finding/ranking. Wraps around the data sturcures provides by the
  * DocumentReader.
  *
  * @param documentReader DocumentReader used for creating the Inverted Index.
  */
class InvertedIndex(documentReader: DocumentReader) {
  private val logger = new Logger("InvertedIndex")

  /**
    * Maps a word to the position in the dictionary.
    */
  val dictionary: Map[String, Int] = documentReader.dictionary

  /**
    * Provides a list of WordInDocInfo for a given word (inverted Index).
    */

  def getDocsForWords(words: Iterable[String])
  = words.flatMap(w =>
                    documentReader.queryInvertedIndex(w).map(t =>
                                                          WordInDocInfo(w, documentReader
                                                            .documentInfo(t._1)._2, t._1, t._2, t._3)))


  def getDocLength(docId: Int): Int =
    documentReader.documentInfo(docId)._1

  def getTotalNumberOfWords: Int = documentReader.totalNumberOfWords

  def getDocCount: Int = documentReader.docCount

  def getWordCount(word: String): WordCount = documentReader.wordCounts(word)

}

//TODO return types


class PassThroughInvertedIndex(documentReader: PassThroughDocumentReader) extends InvertedIndex(documentReader) {

  override def getDocsForWords(words: Iterable[String]) = {
    val wordSet = words.toSet
    documentReader.tipster.stream.take(documentReader.docCount).zipWithIndex.flatMap(x => documentReader.docToWords(x
                                                                                                                      ._1, x
                                                                                                                      ._2))
      .filter(w
              => wordSet
          .contains(w.word))
  }

}