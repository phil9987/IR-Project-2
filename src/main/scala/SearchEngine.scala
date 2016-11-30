object SearchEngine{

  def main(args: Array[String]): Unit ={
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp)
    val ii = new DefaultInvertedIndex(dr)
    val rm = new DefaultRankingModel(ii, wp)
    val logger = new Logger("SearchEngine")

    rm.query("the cat jumps asdf elephant".split(' ').toList)
    println(QueryMetric.codeToQuery(50))

    for(queryId <- QueryMetric.codeToQuery.keys)
      {
        val query = QueryMetric.codeToQuery(queryId)
        val ranking = rm.query(query.split(' ').toList)
        //TODO evalute
      }

  }
}