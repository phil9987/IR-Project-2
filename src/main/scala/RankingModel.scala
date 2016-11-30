/**
  * Created by marc on 29/11/16.
  */
abstract class RankingModel(invertedIndex: InvertedIndex) {


  /**
    * Queries the model for a given list of words.
    * Implementations of this should:
    * - Use the InvertedIndex's dictionary to convert the query into a vector.
    * - Load relevant words, and their documents from the inverted index.
    * - Perform calculations based on the model.
    * - Rank the documents according
    * - Return the list of ranked documentIds
    * @param query List of query words.
    * @return A ranked List of documentIds.
    */
  abstract def query(query: List[String]): List[Int]

}
