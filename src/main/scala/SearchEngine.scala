object SearchEngine{

  def main(args: Array[String]): Unit ={
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp)
    val ii = new DefaultInvertedIndex(dr)
    val rm = new VectorSpaceModel(ii, wp, dr)
    val logger = new Logger("SearchEngine")


    rm.query("market entry".split(' ').toList)
    /*
    println(QueryMetric.codeToQuery(50))

    for(queryId <- QueryMetric.codeToQuery.keys)
      {
        val query = QueryMetric.codeToQuery(queryId)
        val ranking = rm.query(query.split(' ').toList)
        //TODO evalute
      }
   */
  }
}