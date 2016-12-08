import ch.ethz.dal.tinyir.processing.Tokenizer

/**
  * Created by marc on 29/11/16.
  */


/**
  * The inverted Index used for Document finding/ranking. Wraps around the data sturcures provides by the
  * DocumentReader.
  * @param documentReader DocumentReader used for creating the Inverted Index.
  */
class InvertedIndex(documentReader: DocumentReader) {
  private val logger = new Logger("InvertedIndex")

  /**
    * Maps a word to the position in the dictionary.
    */
  val dictionary : Map[String, Int] = documentReader.dictionary

  /**
    * Provides a list of WordInDocInfo for a given word (inverted Index).
    */
  private val invertedIndex : Map[String, List[WordInDocInfo]] = documentReader.invertedIndex.toMap

  def getDocsForWords(words: Iterable[String])  = words.flatMap(invertedIndex(_))

  def getDocLength(docName: String) : Int =
    documentReader.documents(docName).tokens.length

  def getTotalNumberOfWords : Int = documentReader.totalNumberOfWords

  def getDocCount : Int = documentReader.docCount

  def getWordCount(word :String) : WordCount = documentReader.wordCounts(word)

}

//TODO return types


class PassThroughInvertedIndex(documentReader: DocumentReader) extends InvertedIndex(documentReader)
{

  override def getDocsForWords(words: Iterable[String])  = {
    val wordSet = words.toSet
    documentReader.documents.values.flatMap( documentReader.docToWords ).filter(w => wordSet.contains(w.word))
  }

}