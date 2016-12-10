/**
  * The inverted Index used for Document finding/ranking. Wraps around the data structures provides by the
  * DocumentReader.
  *
  * @param documentReader DocumentReader used for creating the Inverted Index.
  */
class InvertedIndex(documentReader: DocumentReader) {

  /**
    * Maps a word to the position in the dictionary.
    */
  val dictionary: Map[String, Int] = documentReader.dictionary

  /**
    * For a given list of words, retrieve their WordInDocInfo from the inverted index.
    *
    * @param words The words to be looked up.
    * @return A list of WordInDocInfos for all of the words.
    */
  def getDocsForWords(words: Iterable[String]): List[WordInDocInfo]
  = words.flatMap(w =>
                    documentReader.queryInvertedIndex(w).map(t =>
                                                               WordInDocInfo(w, documentReader
                                                                 .documentInfo(t._1)._2, t._1, t._2, t._3))).toList


  /**
    * For a given documentId retrieve the length of the document.
    *
    * @param docId The documentId.
    * @return The document length.
    */
  def getDocLength(docId: Int): Int = documentReader.documentInfo(docId)._1

  /**
    * The total number of words read.
    *
    * @return The total number of words read.
    */
  def getTotalNumberOfWords: Int = documentReader.totalNumberOfWords

  /**
    * The total number of documents read.
    *
    * @return The total number of documents read.
    */
  def getDocCount: Int = documentReader.docCount

  /**
    * For a given word return its WordCount.
    *
    * @param word A word in the dictionary.
    * @return The WordCount (total occurrences, nr of documents containing it) for the given word.
    */
  def getWordCount(word: String): WordCount = documentReader.wordCounts(word)

}

/**
  * This provides the same interface as an inverted index, but performs lookups for all queries by going through the
  * documents. Does not create an inverted index but uses the same support structures for documentId -> documentName
  * resolution etc.
  *
  * @param documentReader DocumentReader used for creating the Inverted Index.
  */
class PassThroughInvertedIndex(documentReader: PassThroughDocumentReader) extends InvertedIndex(documentReader) {

  /**
    * For a given list of words, retrieve their WordInDocInfo from the documents. Does NOT perform look-up in an
    * inverted index, but passes through all documents to do so.
    *
    * @param words The words to be looked up.
    * @return A list of WordInDocInfos for all of the words.
    */
  override def getDocsForWords(words: Iterable[String]): List[WordInDocInfo] = {
    val wordSet = words.toSet
    documentReader.tipster.stream.take(
      documentReader.docCount).zipWithIndex.flatMap(x =>
                                                      documentReader.docToWords(x._1, x._2))
      .filter(w => wordSet.contains(w.word)).toList
  }

}