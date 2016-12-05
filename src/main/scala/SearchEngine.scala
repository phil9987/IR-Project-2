object SearchEngine{

  def main(args: Array[String]): Unit ={

    var verbose = false
    var modelType = "language"
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp)
    val ii = new DefaultInvertedIndex(dr)
    val rm = if (modelType == "language"){new LanguageModel(ii, wp, dr)} else {new VectorSpaceModel(ii, wp, dr)}
    val logger = new Logger("SearchEngine")

    def evaluateModel(verbose : Boolean = false): Double ={
      var MAP = 0.0
      for(queryId <- QueryMetric.codeToQuery.keys)
      {
        if (verbose ) logger.log ("=====================================================")
        val query = QueryMetric.codeToQuery(queryId)
        if (verbose ) println(query.split(' ').toList)
        val ranking = rm.query(query.split(' ').toList, queryId, verbose).map( dr.idToDocinfos(_).docName )
        //println(ranking)
        val metrics =  QueryMetric.eval(queryId, ranking)
        MAP = MAP + metrics._4
        if(verbose) println( s"Query $queryId -> precision: ${metrics._1(100)}, recall: ${metrics._2(100)} , F1: ${metrics._3(100)}, AP: ${metrics._4}" )
      }
      MAP = MAP / QueryMetric.codeToQuery.size
      if (verbose) println(s"MAP: $MAP")
      MAP
    }

    if (modelType == "language"){
      var thetaRange = List(0.55, 0.60,0.65, 7,0.75,0.8)
      var zetaRange = List(0,100,150, 200, 250, 300)
      var fancyHitRange = List(0,1,3,5)
      var totalCombinations = thetaRange.length * zetaRange.length * fancyHitRange.length
      var count = 0

      var scores = scala.collection.mutable.Map[(Double, Int, Double), Double]()
      for( theta <- thetaRange ){
        for( zeta <- zetaRange ){
          for (fh <- fancyHitRange) {
            count += 1
            logger.log(s"Testing hyperparameters... ${100 * count / totalCombinations} percent done!")
            rm.setHyperParameters(theta, zeta, fh)
            scores((theta, zeta, fh)) = evaluateModel()
          }
        }
      }
      logger.log("Best scores for each hyperparameters : ")
      scores.toList.sortBy(- _._2).foreach{  x=> logger.log(s"(theta : ${x._1._1} , zeta ${x._1._2}, fancyHitBonus ${x._1._3}) => ${x._2}")}

      println("DETAILS FOR BEST HYPERPARAMETERS : ")
      val bestParams = scores.toList.sortBy(- _._2).head._1
      rm.setHyperParameters(bestParams._1, bestParams._2, bestParams._3)
      evaluateModel(true)

    }

  }
}