object SearchEngine{
  val logger = new Logger("SearchEngine")
  val verbose = true

  def evaluateTiming() : Unit = {
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp, 10000)
    val ii = new InvertedIndex(dr)
    val rm = new VectorSpaceModel(ii, wp)
    rm.setModelMode("lpn.nnn")
    rm.setHyperParameters(7.0)
    var MAP = 0.0
    val keys = QueryMetric.codeToQuery.keys.toList.take(1)
    for(queryId <- keys)
    {
      val query = QueryMetric.codeToQuery(queryId)
      rm.enqueueBatchQuery(query.split(' ').toList)
    }
    val results = rm.performBatchQuery()
    for(i <- keys.indices )
    {
      val ranking = results(i).rankedDocs
      val queryId = keys(i)
      val metrics =  QueryMetric.eval(queryId, ranking)
      MAP = MAP + metrics._4
    }
    MAP = MAP / QueryMetric.codeToQuery.take(1).size
    println(s"MAP: $MAP")
  }


  def printQueryDetails(queryId : Int, result : QueryResult, N : Int = 30) : Unit = {
        if (verbose) {
          logger.log(s"TOP $N DOCS : ")
          var i = 0
          for (doc <- result.rankedDocs.take(N)) {
            i += 1
            var color = Console.RESET
            if (queryId != -1) {
              val judgement = QueryMetric.codeToJudgement(queryId).filter(x => x._1 == doc)
              if (judgement.isEmpty) {
                color = Console.YELLOW
              } else if (judgement.head._2 == 1) {
                color = Console.GREEN
              }
              else {
                color = Console.RED
              }
            }
            //TODO (${r.documents(doc).tokens.length}
            logger.log(s"$color TOP $i : $doc  tokens) => ${result.docToWordMap(doc)
              .sortBy(_.word)
              .map(x
                                                                                                                        => s"${x.word}: ${x.numOccurrence}").mkString(",")}")
            print(Console.RESET)
          }
        }
  }



  def main(args: Array[String]): Unit ={
//    evaluateTiming()
    val logger = new Logger("SearchEngine")
    if (args.length == 0 || ( !Set("tf","language").contains(args(0)))){
      logger.log(s"${Console.RED} Please specify a valid model as first argument (either language or tf) ${Console.RESET}")
      return
    }
    var skipTuning = args.contains("--quick")
    var beQuiet = args.contains("--quiet")

    var verbose = false
    var modelType = args(0)

    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp, 10000)
    val ii = new InvertedIndex(dr)
    val rm = if (modelType == "language"){new LanguageModel(ii, wp)} else {new VectorSpaceModel(ii, wp)}

    def evaluateModel(verbose : Boolean = false): Double ={
      var MAP = 0.0
      for(queryId <- QueryMetric.codeToQuery.keys.take(1))
      {
        if (verbose ) logger.log ("=====================================================")
        val query = QueryMetric.codeToQuery(queryId)
        if (verbose ) println(query.split(' ').toList)
        val result = rm.query(query.split(' ').toList)
        printQueryDetails(queryId, result, 30)
        //println(ranking)
        val metrics =  QueryMetric.eval(queryId, result.rankedDocs)
        MAP = MAP + metrics._4
        if(verbose) println( s"Query $queryId -> precision: ${metrics._1(100)}, recall: ${metrics._2(100)} , F1: ${metrics._3(100)}, AP: ${metrics._4}" )
      }
      MAP = MAP / QueryMetric.codeToQuery.take(1).size
      if (verbose) println(s"MAP: $MAP")
      MAP
    }

    if (modelType == "language"){
      var bestParams = (0.55,150,3.0)
      if(!skipTuning) {
        var thetaRange = List(0.55, 0.60, 0.65, 0.7, 0.75, 0.8)
        var zetaRange = List(100, 150, 200, 250, 300)
        var fancyHitRange = List(0.0, 1.0, 3.0, 5.0, 7.0)
        var totalCombinations = thetaRange.length * zetaRange.length * fancyHitRange.length
        var count = 0

        var scores = scala.collection.mutable.Map[(Double, Int, Double), Double]()
        for (theta <- thetaRange) {
          for (zeta <- zetaRange) {
            for (fh <- fancyHitRange) {
              count += 1
              logger.log(s"Tuning hyperparameters... ${100 * count / totalCombinations} percent done!")
              rm.setHyperParameters(theta, zeta, fh)
              scores((theta, zeta, fh)) = evaluateModel()
            }
          }
        }
        logger.log("Best scores for each hyperparameters : ")
        scores.toList.sortBy(-_._2).foreach { x => logger.log(s"(theta ${x._1._1} , zeta ${x._1._2}, fancyHitBonus ${x._1._3}) => ${x._2}") }

        println("DETAILS FOR BEST HYPERPARAMETERS : ")
        bestParams = scores.toList.sortBy(-_._2).head._1
      }
      rm.setHyperParameters(bestParams._1, bestParams._2, bestParams._3)
      evaluateModel(true)

    }
    else if (modelType == "tf"){
      var bestParams = ("lpn.nnn", 7.0)
      if(!skipTuning) {
        var count = 0
        var docsModesList = List("nnn", "nnc", "ntn", "ntc", "npn", "npc",
          "lnn", "lnc", "ltn", "ltc", "lpn", "lpc",
          "bnn", "bnc", "btn", "btc", "bpn", "bpc")

        var queryModesList = List("nnn")
        var fancyHitRange = List(0.0, 5.0, 7.0, 10.0, 15.0)

        var scores = scala.collection.mutable.Map[(String, Double), Double]()
        var totalCombinations = docsModesList.length * queryModesList.length * fancyHitRange.length
        for (docMode <- docsModesList) {
          for (queryMode <- queryModesList) {
            for (fancyHitBonus <- fancyHitRange) {
              count += 1
              logger.log(s"Tuning hyperparameters... ${100 * count / totalCombinations} percent done!")
              rm.setModelMode(docMode + "." + queryMode)
              rm.setHyperParameters(fancyHitBonus)
              scores((docMode + "." + queryMode, fancyHitBonus)) = evaluateModel()
            }
          }
        }
        logger.log("Best scores for each hyperparameters : ")
        scores.toList.sortBy(-_._2).foreach { x => logger.log(s"${x._1} => ${x._2}") }

        println("DETAILS FOR BEST HYPERPARAMETERS : ")
        bestParams = scores.toList.sortBy(-_._2).head._1
      }
      rm.setModelMode(bestParams._1)
      rm.setHyperParameters(bestParams._2)
      evaluateModel(true)

    }
    else{
      logger.log("Unknown model name. Use 'vectorspace' or 'language'")
    }
  }
}