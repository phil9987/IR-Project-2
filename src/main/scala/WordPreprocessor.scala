import com.github.aztek.porterstemmer.PorterStemmer
import ch.ethz.dal.tinyir.processing.StopWords

import scala.collection.immutable.HashMap
/**
  * Created by marc on 30/11/16.
  */


class WordPreprocessor {

  //cache used by cachedStem
  protected val stemmingCache = scala.collection.mutable.HashMap[String, String]()

  // patern used in filterWords to remove words which don't contain any character
  protected val pattern = "[\\p{L}\\-]+".r.pattern

  protected val abbreviations = collection.mutable.Map[String, String]();
  abbreviations.put("USA", "united-states-america");
  abbreviations.put("US", "united-states-america");
  abbreviations.put("U.S.", "united-states-america");
  abbreviations.put("U.S.A", "united-states-america");
  abbreviations.put("United States of America", "united-states-america");
  abbreviations.put("America", "united-states-america");

  def unifyAbbreviations(token:String) : String = {
    abbreviations.getOrElse(token,token)
  }

  /**
    * Translate a token to its stemmed base word.
    * Uses a cache (HashMap) in order not to duplicate calculations.
    * @param token The token to be stemmed
    * @return stemmed word.
    */
  def cachedStem(token: String) : String = {
    if (!stemmingCache.contains(token)) stemmingCache(token) = PorterStemmer.stem(token)
    stemmingCache(token)
  }

  /**
    * Filters words.
    * Current implementation removes stopwords and words not made up from letters and '-'.
    * @param word The word to be filtered.
    * @return Boolean indicating whether to remove the word or not.
    */
  def filterWords(word: String) = !StopWords.stopWords.contains(word) && pattern.matcher(word).matches()

  /**
    * turns every letter to lowercase, removes stopwords and words not made up from letters or '-'.
    * @param tokens list of tokens
    * @return preprocessed list of tokens
    */
  def preprocess(tokens : List[String]) : List[String] =
    tokens.map(unifyAbbreviations).map(_.toLowerCase).filter(filterWords(_)).map(cachedStem)
}
