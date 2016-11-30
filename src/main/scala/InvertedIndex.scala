/**
  * Created by marc on 29/11/16.
  */
abstract class InvertedIndex {

  /**
    * Maps a word to the position in the dictionary.
    */
  abstract val dictionary : Map[String, Int]

  /**
    * Provides a list of DocInfos for a given word (inverted Index).
    */
  abstract val invertedIndex : Map[String, List[DocInfo]]

}
