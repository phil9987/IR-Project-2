object SearchEngine{

  def main(args: Array[String]): Unit ={
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
    val dr = new DocumentReader(wp)
    val ii = new DefaultInvertedIndex(dr)
    val rm = if (modelType == "language"){new LanguageModel(ii, wp, dr)} else {new VectorSpaceModel(ii, wp, dr)}

    def evaluateModel(verbose : Boolean = false): Double ={
      var MAP = 0.0
      for(queryId <- QueryMetric.codeToQuery.keys)
      {
        if (verbose ) logger.log ("=====================================================")
        val query = QueryMetric.codeToQuery(queryId)
        if (verbose ) println(query.split(' ').toList)
        val ranking = rm.query(query.split(' ').toList, queryId, verbose&&(!beQuiet)).map( dr.idToDocinfos(_).docName )
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
      var bestParams = ("lpn.nnn", 10.0)
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