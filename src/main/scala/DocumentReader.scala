import java.io.{File, InputStream}

import ch.ethz.dal.tinyir.processing.{TipsterParse, XMLDocument}
import ch.ethz.dal.tinyir.io.TipsterStream
//import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}

/**
  * extends TipsterParse class of provided TinyIR library by also parsing the title (marked with HEAD) of the tipster
  * articles.
  * @param is InputStream
  */
class TipsterParsePlus(is: InputStream) extends TipsterParse(is){
  override def title : String = read(doc.getElementsByTagName("HEAD"))
  override def content: String = title + " " + body
}

/**
  * extends TipsterStream class of provided TinyIR library by using TipsterParsePlus (which also parses the titles)
  * @param path the path of the folder containing the zip-file(s) with the tipster-articles
  * @param ext (optional) the extension, necessary if not zip
  */
class TipsterStreamPlus(path: String, ext: String = "") extends TipsterStream(path, ext){
  override def stream : Stream[XMLDocument] = unparsed.stream.map(is => new TipsterParsePlus(is))
}

/**
  * Information to a document about a word
  * @param docNb - document identifier
  * @param numOccurrence - total number of occurrences of word in document
  * @param isInHeader - occurs this word (at least once) in the header of the document?
  */
case class WordInfo(docNb: Int, numOccurrence: Int, isInHeader: Boolean)

/**
  * Adds the word to the Wordinfo. Used in RankingModel
 */
case class ExtendedWordInfo(word: String, docNb : Int, numOccurrence : Int, isInHeader : Boolean)

/**
  * Information to a document
  * @param docName name of document (like it's parsed)
  * @param numWords total number of words in document
  */
case class DocInfo(docName: String, numWords: Int)

/**
  * Holds the corpus-wide counts for a certain word
  * @param docCount
  * @param frequencyCount
  */
case class WordCount(docCount: Int, frequencyCount: Int)

/**
  * Base class for the reader.
  * Implements structure and functions common to all implementation of Reader.
  */
class DocumentReader(preprocessor: WordPreprocessor){
  private val logger = new Logger("BaseReader")
  val wordCounts = scala.collection.mutable.HashMap[String, WordCount]()
  var docCount = 0
  val postings = new scala.collection.mutable.HashMap[String, List[WordInfo]].withDefaultValue(Nil)
  val idToDocinfos = new scala.collection.mutable.HashMap[Int, DocInfo];
  var dictionary : Map[String, Int] = null

  protected def init() = {
    logger.log("init")
    logger.log("init: Initializing Stream.")
    val tipster = new TipsterStreamPlus(new File("./src/main/resources").getCanonicalPath, ".zip")
    docCount = tipster.length
    logger.log("init: Number of files in zips = " + docCount)
    logger.log("init: Counting word-occurrences in corpus...")

    //TODO: term-frequency over whole collection
    var docNb = 0
    for (doc <- tipster.stream.take(30000)) {
      logger.log(s"Reading document ${docNb }", "BaseReader", 10000)
      idToDocinfos(docNb) = new DocInfo(doc.name, doc.tokens.length)
      val words = preprocessor.preprocess(doc.tokens)
      words.groupBy(identity).mapValues(_.size).toList.foreach{ case (word, count) =>
          val wc = wordCounts.getOrElse(word, new WordCount(0,0))
          wordCounts(word) = new WordCount(wc.docCount + 1, wc.frequencyCount + count)
          postings(word) ::= new WordInfo(docNb, count, false)
      }
      docNb += 1
    }
    dictionary = wordCounts.keys.toList.sorted.zipWithIndex.toMap
    logger.log(s"init: Dictionary size: to ${dictionary.size}")
  }

  init()
}