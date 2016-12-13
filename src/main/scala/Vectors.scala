import org.iq80.leveldb.DB
import java.io._
import java.nio.ByteBuffer
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.lang.Double.doubleToLongBits
import ch.ethz.dal.tinyir.processing.{TipsterParse, Tokenizer, XMLDocument}
import ch.ethz.dal.tinyir.io.TipsterStream
import org.iq80.leveldb.{DB, Options}

import scala.collection.mutable.{HashMap => MutHashMap}

object Vectors{

  var ii : InvertedIndex = _
  type TermVector = List[Double]

  var levelDBOptions : Options = new Options()
  var levelDBFileName = "VECTORNORMS"

  var db: DB =  org.iq80.leveldb.impl.Iq80DBFactory.factory.open(new File(levelDBFileName), levelDBOptions)
  var logger = new Logger("VectorNorms")

  val vectorTypes = List("nn","nt", "np", "ln", "lt","lp", "bn","bt","bp")

  def calculateNorm(infos : List[WordInDocInfo], vectorType : String) : Double = {
    assert(vectorType.length == 2)
    var tfMode = vectorType(0)
    var idfMode = vectorType(1)
    math.sqrt(infos.map(x => tf(x, tfMode, 7.0) * idf(x, idfMode)).map(x=> x*x).sum)
  }

  def toByteArray(value : Double) : Array[Byte] = {
    val bytes = Array.fill[Byte](8)(0)
    ByteBuffer.wrap(bytes).putDouble(value)
    return bytes
  }
  def toDouble(bytes : Array[Byte]) : Double = {
    return ByteBuffer.wrap(bytes).getDouble()
  }

  def saveNorms(): Unit ={
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

    val tipster = new TipsterStreamPlus(new File("./src/main/resources").getCanonicalPath, ".zip")
    var docId = 0
    val batch = db.createWriteBatch()
    for (doc <- tipster.stream) {
      logger.log(s"Saving norms for document $docId", "readingDocNr", 5000)
      val wordInfos = ii.reader.docToWords(doc, docId)
      for ( v <- vectorTypes){
        val key = docId.toString + v
        val value = toByteArray(calculateNorm(wordInfos, v))
        batch.put(key.getBytes, value)
      }
      docId += 1
    }
    db.write(batch)
    batch.close()
  }

  // Remembers how the model represents the document vector
  var docVectorRepresentation : String = ""
  /**
    * Retrieves the norm for the given doc from the DB.
    * Uses the saved docVectorReprentation to know which norm to retrieve
    * @param docId For which document to retrieve the norm
    * @return : The norm to return (as Double)
   */
  def retrieveNormFromDb(docId : Int): Double ={
    assert(vectorTypes.contains(docVectorRepresentation))
    toDouble(db.get((docId.toString + docVectorRepresentation).getBytes))
  }

  /**
    * Calculates the term frequency as Double
    * @param info  the WordInDocInfo for which to calculate the term frequency
    * @param tfMode how to calculate the term frequency (n => natural, l=>logarithmic, b=> boolean)
    * @return a double representing the calculated tf
    */
  def tf(info: WordInDocInfo, tfMode: Char, fancyHitBonus : Double = 0.0): Double = {
    assert(tfMode == 'n' || tfMode == 'l' || tfMode == 'b')
    var occurrence  : Double = info.numOccurrence

    if(info.isInHeader) occurrence += fancyHitBonus

    if (tfMode == 'n')
      occurrence
    else if (tfMode == 'l') {
      if (occurrence == 0)
        0
      else
        1 + math.log(occurrence)
    }
    else {
      assert(tfMode == 'b')
      if (occurrence == 0)
        0
      else
        1
    }
  }

  /**
    * Calculates the inverse Document frequency as Double. used the Inverted index as information source
    * @param info the WordInDocInfo for which to calculate the inverse document frequency
    * @param idfMode how to calculate the idf (n=> none, t=> normal idf, p=> prob idf)
    * @return the calculated idf as double
    */
  def idf(info: WordInDocInfo, idfMode: Char): Double = {
    assert(idfMode == 'n' || idfMode == 't' || idfMode == 'p')
    if (idfMode == 'n')
      1
    else if (idfMode == 't') {
      math.log(ii.getDocCount / ii.getWordCount(info.word).docCount)
    }
    else {
      assert(idfMode == 'p')
      math.max(0, math.log((ii.getDocCount - ii.getWordCount(info.word)
        .docCount) / ii.getWordCount(info.word).docCount))
    }
  }

  /**
    * Normalizes the vector
    * @param v The vector to normalize
    * @param docId Which doc the vector corresponds to (used to fetch norm in DB).
    *              -1 means the vector corresponds to a query.
    * @param normMode How to normalize (n=> no normalization, c => cosine normalization)
    * @return The normalized vector
    */
  def normalize(v: TermVector, docId: Int, normMode : Char ): TermVector = {
    assert(normMode == 'n' || normMode == 'c')
    if (normMode == 'n')
      v
    else {
      assert(normMode == 'c')
      val divisor = if(docId == -1) math.sqrt(v.map(x => x * x).sum)  else retrieveNormFromDb(docId)
      return  v.map(_ / divisor)

    }
  }

  /**
    * Calculates the dot product between two vectors
    * @param x the first vector, usually represents document
    * @param y the second vector, usually represents query
    * @return Double, the dot product x * y
    */
  def dotProduct(x : TermVector, y: TermVector) : Double = {
    assert(x.length == y.length)
    (x zip y).map(x => x._1 * x._2).sum
  }

  /**
    * Interface used by the ranking model to get the score(document, query)
    * @param infoList the list of relevant WordInDocInfos for the document
    * @param query the list of terms appearing in query
    * @param modelMode which vecotr representation is used (example : ltn.nnc )
    */
  def score(infoList: List[WordInDocInfo], query : List[String], modelMode : String, fancyHitBonus : Double) : Double = {
    docVectorRepresentation = modelMode.substring(0,2)
    val docVector : TermVector = infoList.sortBy(_.word)
      .map(info => tf(info, modelMode(0), fancyHitBonus) * idf(info, modelMode(1)))
    val queryVector : TermVector= query.sorted.map(word => WordInDocInfo(word, infoList.head.docName, infoList
      .head.docId, 1, isInHeader = false)
    ).map(info => tf(info, modelMode(4)) * idf(info, modelMode(5)))

    dotProduct(normalize(docVector, infoList.head.docId, modelMode(2)), normalize(queryVector, -1, modelMode(6)))
  }
}

