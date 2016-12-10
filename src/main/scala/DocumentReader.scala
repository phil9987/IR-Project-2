import java.io._
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import ch.ethz.dal.tinyir.processing.{TipsterParse, Tokenizer, XMLDocument}
import ch.ethz.dal.tinyir.io.TipsterStream
import org.iq80.leveldb.{DB, Options}

import scala.collection.mutable.{HashMap => MutHashMap}

//The following two classes extend the Tipster stream and parser so that also the header of the document is read.

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

  /**
    * Instead of just tokenizing the body of the document this replaces the abbreviations in the document body and
    * header and tokenizes it.
    *
    * @return List of tokens.
    */
  override def tokens: List[String] = Tokenizer.tokenize(wp.replaceImportantAbbreviations(content))
}

/**
  * extends TipsterStream class of provided TinyIR library by using TipsterParsePlus (which also parses the titles)
  * see TipsterParsePlus above
  *
  * @param path the path of the folder containing the zip-file(s) with the tipster-articles
  * @param ext  (optional) the extension, necessary if not zip
  */
class TipsterStreamPlus(path: String, ext: String = "") extends TipsterStream(path, ext) {
  override def stream: Stream[XMLDocument] = unparsed.stream.map(is => new TipsterParsePlus(is))
}


//The following case classes define data structures for the DocumentReader and the inverted index

/**
  *
  * @param word          the found word
  * @param docName       document name
  * @param docId         document id
  * @param numOccurrence total number of occurrences of word in document
  * @param isInHeader    occurs this word (at least once) in the header of the document?
  */
case class WordInDocInfo(word: String, docName: String, docId: Int, numOccurrence: Int, isInHeader: Boolean)

/**
  * Holds the corpus-wide counts for a certain word
  *
  * @param docCount       Number of documents that the word occurs in.
  * @param frequencyCount Number of times the word occurs over all documents.
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
  /**
    * For each word keep the count of it's occurrence.
    */
  val wordCounts = MutHashMap[String, WordCount]()
  /**
    * Inverted Index.
    * To save memory the inverted index only stores (documentId, numOccurrence, isInHeader) for each hit of a word in
    * a document.
    */
  val invertedIndex = new MutHashMap[String, List[(Int, Int, Boolean)]].withDefaultValue(Nil)
  val tipster = new TipsterStreamPlus(new File("./src/main/resources").getCanonicalPath, ".zip")
  val docCount = if (maxNrDocs == 0) tipster.length else math.min(maxNrDocs, tipster.length)
  var totalNumberOfWords = 0
  /**
    * DocumentId -> (Length, DocumentName)
    */
  val documentInfo = MutHashMap[Int, (Int, String)]()

  /**
    * Query the inverted Index for a list of (documentId, numOccurrence, isInHeader) for a given word.
    *
    * @param word The word.
    * @return List of (documentId, numOccurrence, isInHeader) for the given word.  
    */
  def queryInvertedIndex(word: String): List[(Int, Int, Boolean)] = invertedIndex(word)

  /**
    * Takes the words of a document and converts them to WordInDocInfos.
    *
    * @param doc   The document.
    * @param docId The id of the document
    * @return List of WordInDocInfos for all words in the document.
    */
  def docToWords(doc: XMLDocument, docId: Int): List[WordInDocInfo] = {
    val titleWords = preprocessor.preprocess(Tokenizer.tokenize(doc.title)).distinct
    val words = preprocessor.preprocess(doc.tokens)
    words.groupBy(identity).mapValues(_.size).toList.map { case (word, count) =>
      WordInDocInfo(word, doc.name, docId, count, titleWords.contains(word))
                                                         }
  }

  /**
    * Initializes the dictionary, wordCounts and invertedIndex by passing through the documents.
    */
  protected def init() = {
    logger.log("init: Initializing Stream.")
    logger.log(s"init: Number of files in zips = ${tipster.length}, reading $docCount")
    var docId = 0
    for (doc <- tipster.stream.take(docCount)) {

      logger.log(s"Reading document $docId", "readingDocNr", 5000)
      documentInfo(docId) = (doc.tokens.length, doc.name)
      val wordInfos = docToWords(doc, docId)
      totalNumberOfWords += wordInfos.map(_.numOccurrence).sum

      wordInfos.foreach { w =>
        val wc = wordCounts.getOrElse(w.word, WordCount(0, 0))
        wordCounts(w.word) = WordCount(wc.docCount + 1, wc.frequencyCount + w.numOccurrence)
        invertedIndex(w.word) ::= (docId, w.numOccurrence, w.isInHeader)
                        }
      docId += 1
    }
    logger.log(s"init: Total number of words: $totalNumberOfWords")
  }


  init()
  val dictionary = wordCounts.keys.toList.sorted.zipWithIndex.toMap
  logger.log(s"dictionary size: ${dictionary.size}")
}

/**
  * Similar to DocumentReader, but does not build an inverted index.
  * @param preprocessor Preprocessor to be used when reading words.
  * @param maxNrDocs    The maximum of words to be read. If 0 is passed all documents are read. The argument is optional
  *                     and the default value is 0.
  */
class PassThroughDocumentReader(preprocessor: WordPreprocessor,
                                maxNrDocs: Int = 0) extends DocumentReader(preprocessor, maxNrDocs) {

  /**
    * Do the same as base version, but don't create inverted index.
    */
  override def init() = {
    logger.log("init: Initializing Stream, pass through mode")
    logger.log(s"init: Number of files in zips = ${tipster.length}, reading $docCount")

    var docId = 0
    for (doc <- tipster.stream.take(docCount)) {
      logger.log(s"Reading document $docId", "readingDocNr", 5000)
      documentInfo(docId) = (doc.tokens.length, doc.name)
      val wordInfos = docToWords(doc, docId)
      totalNumberOfWords += wordInfos.map(_.numOccurrence).sum
      wordInfos.foreach { w =>
        val wc = wordCounts.getOrElse(w.word, WordCount(0, 0))
        wordCounts(w.word) = WordCount(wc.docCount + 1, wc.frequencyCount + w.numOccurrence)
                        }
      docId += 1
    }
    logger.log(s"init: Total number of words: $totalNumberOfWords")
  }
}

/**
  * Same as DocumentReader but stores the inverted index in levelDB.
  * @param preprocessor Preprocessor to be used when reading words.
  * @param maxNrDocs    The maximum of words to be read. If 0 is passed all documents are read. The argument is optional
  *                     and the default value is 0.
  */
class LevelDBDocumentReader(preprocessor: WordPreprocessor,
                            maxNrDocs: Int = 0) extends DocumentReader(preprocessor, maxNrDocs) {

  var levelDBOptions: Options = _
  var levelDBFileName: String = _
  var db: DB = _

  /**
    * Query inverted index for the given word. Calls the DB.
    * @param word The word.
    * @return List of (documentId, numOccurrence, isInHeader) for the given word.
    */
  override def queryInvertedIndex(word: String) : List[(Int, Int, Boolean)] = fromInvertedIndexDB(word)

  /**
    * Load the inverted index for a given word from the DB. Uses the same format as the invertedIndex HashMap as this
    * allows to use this as a drop-in replacement.
    * @param word The word.
    * @return List of (documentId, numOccurrence, isInHeader) for the given word.
    */
  def fromInvertedIndexDB(word: String): List[(Int, Int, Boolean)] = {
    val res = db.get(word.getBytes)
    if (res == null) List() //if word is not in DB
    else {
      val ois = new ObjectInputStream(new ByteArrayInputStream(db.get(word.getBytes)))
      val value = ois.readObject
      ois.close()
      value.asInstanceOf[List[(Int, Int, Boolean)]]
    }
  }

  /**
    * Adds the current invertedIndex to the inverted index. Takes into account the existing state of the database.
    * For serialization we use ObjectOutputStream, which is not ideal in terms of size, but sufficient.
    * Deletes whatever is stored in invertedIndex after saving it to the DB and suggests Java to perform garbage
    * collection.
    */
  def writeToDB() = {
    logger.log("writing to DB")
    val batch = db.createWriteBatch()
    for (key <- invertedIndex.keys) {
      val current = fromInvertedIndexDB(key)
      val newList = invertedIndex(key) ++ current
      val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(stream)
      oos.writeObject(newList)
      oos.close()
      batch.put(key.getBytes, stream.toByteArray)
    }
    db.write(batch)
    batch.close()
    logger.log("done cleaning")
    invertedIndex.clear() //delete in-memory inverted-index
    System.gc() // suggest system to garage collect now
  }

  /**
    * Initializes the dictionary, wordCounts and invertedIndex by passing through the documents.
    */
  override protected def init() = {
    logger.log("init: Initializing Stream.")
    logger.log(s"init: Number of files in zips = ${tipster.length}, reading $docCount")
    levelDBOptions = new Options()
    levelDBOptions.createIfMissing(true)
    levelDBFileName = s"${docCount}_DB"

    //delete the DB if it already exists
    //for development it makes sense to always recreate the DB, but in a deployment it could be easily reused
    if (Files.exists(Paths.get(levelDBFileName))) {
      logger.log("found existing index DB - deleting it")
      Files.walkFileTree(Paths.get(levelDBFileName), new SimpleFileVisitor[Path]() {
        override def visitFile(file: Path,
                               attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path,
                                        exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
    }
    //create new DB
    db = org.iq80.leveldb.impl.Iq80DBFactory.factory.open(new File(levelDBFileName), levelDBOptions)

    var docId = 0
    for (doc <- tipster.stream.take(docCount)) {
      logger.log(s"Reading document $docId", "readingDocNr", 5000)
      if (docId % 10000 == 0) writeToDB() //every 10000 files write the current inverted index into the database
      documentInfo(docId) = (doc.tokens.length, doc.name)
      val wordInfos = docToWords(doc, docId)
      totalNumberOfWords += wordInfos.map(_.numOccurrence).sum
      wordInfos.foreach { w =>
        val wc = wordCounts.getOrElse(w.word, WordCount(0, 0))
        wordCounts(w.word) = WordCount(wc.docCount + 1, wc.frequencyCount + w.numOccurrence)
        invertedIndex(w.word) ::= (docId, w.numOccurrence, w.isInHeader)
                        }
      docId += 1
    }
    writeToDB()
    logger.log(s"init: Total number of words: $totalNumberOfWords")
  }


}