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
  * Base class for the reader.
  * Implements structure and functions common to all implementation of Reader.
  */
class DocumentReader(){
  private val logger = new Logger("BaseReader")

  protected val wordCounts = scala.collection.mutable.HashMap[String, Int]()
  var docCount = 0

  protected def init() = {
    logger.log("init")
    logger.log("init: Initializing Stream.")
    val tipster = new TipsterStreamPlus(new File("./src/main/resources").getCanonicalPath, ".zip")
    docCount = tipster.length
    logger.log("init: Number of files in zips = " + docCount)
    logger.log("init: Counting word-occurences in corpus...")

    for (doc <- tipster.stream.take(20)) {
      doc.tokens.distinct.foreach(x => wordCounts(x) = 1 + wordCounts.getOrElse(x, 0))
    }

  }

  init()
}