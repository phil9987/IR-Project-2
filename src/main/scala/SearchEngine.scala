object SearchEngine{

  def main(args: Array[String]): Unit ={
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp)
    val ii = new DefaultInvertedIndex(dr)
    val rm = new VectorSpaceModel(ii, wp, dr)
    val logger = new Logger("SearchEngine")


//    rm.query("the quick brown fox jumps over the lazy dog".split(' ').toList)

    var MAP = 0.0
    for(queryId <- QueryMetric.codeToQuery.keys)
      {
        val query = QueryMetric.codeToQuery(queryId)
        println(query.split(' ').toList)
        val ranking = rm.query(query.split(' ').toList).map( dr.idToDocinfos(_).docName )
        println(ranking)
        val metrics =  QueryMetric.eval(queryId, ranking)
        MAP = MAP + metrics._4
        println( s"Query $queryId -> precision: ${metrics._1(100)}, recall: ${metrics._2(100)} , F1: ${metrics._3(100)
        }, AP: ${metrics
          ._4}" )

      }
    MAP = MAP / QueryMetric.codeToQuery.size
    println(s"MAP: $MAP")
  }
}