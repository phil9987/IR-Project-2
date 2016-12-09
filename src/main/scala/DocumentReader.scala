import java.io.{File, InputStream}

import ch.ethz.dal.tinyir.processing.{TipsterParse, XMLDocument, Tokenizer}
import ch.ethz.dal.tinyir.io.TipsterStream
import scala.collection.mutable.{HashMap => MutHashMap}

//The following two classes extend the Tipster stream and parser so that also the header of the document is tead.

/**
  * extends TipsterParse class of provided TinyIR library by also parsing the title (marked with HEAD) of the tipster
  * articles.
  *
  * @param is InputStream
  */
class TipsterParsePlus(is: InputStream) extends TipsterParse(is) {
  override def title: String = read(doc.getElementsByTagName("HEAD"))
  override def content: String = title + " " + body
  protected val wp = new WordPreprocessor()

  override def tokens: List[String] = Tokenizer.tokenize(wp.replaceImportantAbbreviations(content))
}

/**
  * extends TipsterStream class of provided TinyIR library by using TipsterParsePlus (which also parses the titles)
  *
  * @param path the path of the folder containing the zip-file(s) with the tipster-articles
  * @param ext  (optional) the extension, necessary if not zip
  */
class TipsterStreamPlus(path: String, ext: String = "") extends TipsterStream(path, ext) {
  override def stream: Stream[XMLDocument] = unparsed.stream.map(is => new TipsterParsePlus(is))
}


//The following case classes define data structures for

/**
  *
  * @param word          - the found word
  * @param docName       - document name
  * @param numOccurrence - total number of occurrences of word in document
  * @param isInHeader    - occurs this word (at least once) in the header of the document?
  */
case class WordInDocInfo(word: String, docName: String, numOccurrence: Int, isInHeader: Boolean)

/**
  * Holds the corpus-wide counts for a certain word
  *
  * @param docCount       Number of documents that the word occurs in.
  * @param frequencyCount Number of times the word occurs over all docments.
  */
case class WordCount(docCount: Int, frequencyCount: Int)

//The document reader

/**
  * Reads words from the documents in src/main/resources/.zip
  *
  * @param preprocessor Preprocessor to be used when reading words.
  * @param maxNrDocs    The maximum of words to be read. If 0 is passed all documents are read. The argument is optional
  *                     and the default value is 0.
  */
class DocumentReader(preprocessor: WordPreprocessor, maxNrDocs: Int = 0) {
  protected val logger = new Logger("DocumentReader")
  val wordCounts = MutHashMap[String, WordCount]()
  val invertedIndex = new MutHashMap[String, List[WordInDocInfo]].withDefaultValue(Nil)
  val documentLength = MutHashMap[String, Int]()
  protected val tipster = new TipsterStreamPlus(new File("./src/main/resources").getCanonicalPath, ".zip")
  val docCount = if (maxNrDocs == 0) tipster.length else math.min(maxNrDocs, tipster.length)
  val docs = tipster.stream.take(docCount)
  var totalNumberOfWords = 0

  /**
    * Takes the words of a document and converts them to WordInDocInfos.
    *
    * @param doc The document.
    * @return List of WordInDocInfos for all words in the document.
    */
  def docToWords(doc: XMLDocument): List[WordInDocInfo] = {
    val titleWords = preprocessor.preprocess(Tokenizer.tokenize(preprocessor.replaceImportantAbbreviations(doc.title))).distinct
    val words = preprocessor.preprocess(doc.tokens)
    words.groupBy(identity).mapValues(_.size).toList.map { case (word, count) =>
      WordInDocInfo(word, doc.name, count, titleWords.contains(word))
                                                         }
  }

  /**
    * Initializes the dictionary, wordCounts and invertedIndex by passing through the documents.
    */
  protected def init() = {
    logger.log("init: Initializing Stream.")
    logger.log(s"init: Number of files in zips = ${tipster.length}, reading $docCount")

    var docNb = 0
    for (doc <- docs) {
      logger.log(s"Reading document $docNb", "readingDocNr", 5000)
      documentLength(doc.name) = doc.tokens.length
      val wordInfos = docToWords(doc)
      totalNumberOfWords += wordInfos.map(_.numOccurrence).sum
      wordInfos.foreach { w =>
        val wc = wordCounts.getOrElse(w.word, WordCount(0, 0))
        wordCounts(w.word) = WordCount(wc.docCount + 1, wc.frequencyCount + w.numOccurrence)
        invertedIndex(w.word) ::= w
                        }
      docNb += 1
    }
    logger.log(s"init: Total number of words: $totalNumberOfWords")
  }

  init()
  val dictionary = wordCounts.keys.toList.sorted.zipWithIndex.toMap
  logger.log(s"dictionary size: ${dictionary.size}")
}

class PassThroughDocumentReader(preprocessor: WordPreprocessor,
                                maxNrDocs: Int = 0) extends DocumentReader(preprocessor, maxNrDocs) {
  override def init() = {
    logger.log("init: Initializing Stream, pass through mode")
    logger.log(s"init: Number of files in zips = ${tipster.length}, reading $docCount")

    var docNb = 0
    for (doc <- docs) {
      logger.log(s"Reading document $docNb", "readingDocNr", 5000)
      documentLength(doc.name) = doc.tokens.length
      val wordInfos = docToWords(doc)
      totalNumberOfWords += wordInfos.map(_.numOccurrence).sum
      wordInfos.foreach { w =>
        val wc = wordCounts.getOrElse(w.word, WordCount(0, 0))
        wordCounts(w.word) = WordCount(wc.docCount + 1, wc.frequencyCount + w.numOccurrence)
                        }
      docNb += 1
    }
    logger.log(s"init: Total number of words: $totalNumberOfWords")
  }
}