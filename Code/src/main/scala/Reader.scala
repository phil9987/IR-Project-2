import java.io.File

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}


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
    val tipster = new TipsterStream(new File("./src/main/resources").getCanonicalPath, ".zip")
    docCount = tipster.length
    logger.log("init: Number of files in zips = " + docCount)
    logger.log("init: Counting word-occurences in corpus...")

    for (doc <- tipster.stream.take(2000)) {
      doc.tokens.distinct.foreach(x => wordCounts(x) = 1 + wordCounts.getOrElse(x, 0))
    }
    
  }

  init()
}