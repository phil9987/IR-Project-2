import com.github.aztek.porterstemmer.PorterStemmer
import ch.ethz.dal.tinyir.processing.StopWords
/**
  * Created by marc on 30/11/16.
  */


class WordPreprocessor {

  //cache used by cachedStem
  protected val stemmingCache = scala.collection.mutable.HashMap[String, String]()

  // patern used in filterWords to remove words which don't contain any character
  protected val pattern = "[\\p{L}\\-]+".r.pattern

  /**
    * Translate a token to its stemmed base word.
    * Uses a cache (HashMap) in order not to duplicate calculations.
    * @param token The token to be stemmed
    * @return stemmed word.
    */
  def cachedStem(token: String) : String = {
    if (!stemmingCache.contains(token)) stemmingCache(token) = PorterStemmer.stem(token.toLowerCase)
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
    tokens.map(_.toLowerCase).filter(filterWords(_)).map(cachedStem)
}
