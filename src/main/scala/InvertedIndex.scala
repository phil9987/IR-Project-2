/**
  * Created by marc on 29/11/16.
  */
abstract class InvertedIndex(documentReader: DocumentReader) {

  /**
    * Maps a word to the position in the dictionary.
    */
  val dictionary : Map[String, Int] = documentReader.dictionary

  /**
    * Provides a list of DocInfos for a given word (inverted Index).
    */
  val invertedIndex : Map[String, List[WordInfo]] = documentReader.postings.toMap
}

class DefaultInvertedIndex(documentReader: DocumentReader) extends InvertedIndex(documentReader)
{
}