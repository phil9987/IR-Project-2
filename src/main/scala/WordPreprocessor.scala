import com.github.aztek.porterstemmer.PorterStemmer

/**
  * Created by marc on 30/11/16.
  */


class WordPreprocessor {

  //cache used by cachedStem
  protected val stemmingCache = scala.collection.mutable.HashMap[String, String]()

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


  def preprocess(tokens : List[String]) : List[String] =
    tokens.map(_.toLowerCase).map(cachedStem)

}
