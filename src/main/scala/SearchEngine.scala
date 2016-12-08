import java.io.File
import java.nio.file.{Files, Paths}

import scala.util.Try

object SearchEngine{
  val logger = new Logger("SearchEngine")


  val bestTfFunctionString = "lpn.nnn"
  val bestTfFHB = 7.0
  val bestLanguageTheta = .55
  val bestLanguageZeta = 150
  val bestLanguageFancyHitRange = 3.0

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

  def readModelSetup() = {
    var modelType = ""
    do {
      println("Run VectorSpace Model or Language Model? [tf, language]")
      modelType = scala.io.StdIn.readLine()
    } while (!(modelType.equals("tf") || modelType.equals("language")))
    modelType
  }

  def modelSetup() = {
    val modelType = readModelSetup()
    if (modelType.equals("tf"))
      readTfSetup()
    else
      readLanguageSetup()
  }

  def readTfSetup() = {
    var functionTypes = ""
    do {
      println(s"TODO description [ [nlp]{r}\\.[nlp]{3}, default = $bestTfFunctionString ] ") //TODO write explaintion
      functionTypes = scala.io.StdIn.readLine()
    } while (!(functionTypes.equals("") || functionTypes.matches("[nlp]{3}\\.[nlp]{3}")  ))
    if (functionTypes.equals("")) functionTypes = bestTfFunctionString
    var fancyHitBonus = ""
    do {
      println(s"TODO description fancy hit bonus [ double, default = $bestTfFHB ] ") //TODO write explaintion
      fancyHitBonus = scala.io.StdIn.readLine()
    } while (!(fancyHitBonus.equals("") || Try {fancyHitBonus.toDouble}.isSuccess )  )
    val fhb : Double = Try {fancyHitBonus.toDouble}.getOrElse(bestTfFHB)
    ("tf", (functionTypes, fhb))
  }

  def readLanguageSetup() = {

    var thetaInput = ""
    do {
      println(s"TODO theta [ double, default = $bestLanguageTheta ] ") //TODO write explaintion
      thetaInput = scala.io.StdIn.readLine()
    } while (!(thetaInput.equals("") || Try {thetaInput.toDouble}.isSuccess )  )
    val theta : Double = Try {thetaInput.toDouble}.getOrElse(bestLanguageTheta)
    var zetaInput = ""
    do {
      println(s"TODO zeta [ int, default = $bestLanguageZeta] ") //TODO write explaintion
      zetaInput  = scala.io.StdIn.readLine()
    } while (!(zetaInput.equals("") || Try {zetaInput.toInt}.isSuccess )  )
    val zeta : Int = Try {zetaInput.toInt}.getOrElse(bestLanguageZeta)
    var fhrInput = ""
    do {
      println(s"TODO fhr [ double, default = $bestLanguageFancyHitRange ] ") //TODO write explaintion
      fhrInput = scala.io.StdIn.readLine()
    } while (!(fhrInput.equals("") || Try {fhrInput.toDouble}.isSuccess )  )
    val fhr : Double = Try {fhrInput.toDouble}.getOrElse(bestLanguageFancyHitRange)
    ("language",(theta,zeta,fhr))
  }

  def evaluateTraining(): Unit =
  {
    val setupInfo = modelSetup()
    val (wp, dr, ii, rm) = setup(setupInfo._1, setupInfo._2)
    evaluateModel(rm, true)
  }

  def interactive(): Unit =
  {
    val setupInfo = modelSetup()
    val (wp, dr, ii, rm) = setup(setupInfo._1, setupInfo._2)
    var query = ""
    while(true)
    {
      print("Enter query: ")
      query = scala.io.StdIn.readLine()
      println(rm.query(query.split(' ').toList).rankedDocs.mkString("[", ", ", "]"))
    }
  }

  def setup(modelType :String, modelParameter : Serializable) : (WordPreprocessor, DocumentReader, InvertedIndex, RankingModel)
  = {
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp, 10000) //TODO remove 10000
    val ii = new InvertedIndex(dr)
    val rm = if (modelType == "language"){new LanguageModel(ii, wp)} else {new VectorSpaceModel(ii, wp)}
    if (modelType == "language")
    {
      val par = modelParameter.asInstanceOf[(Double, Int, Double)]
      rm.setHyperParameters(par._1, par._2, par._3)
    } else
    {
      val par = modelParameter.asInstanceOf[(String, Double)]
      rm.setModelMode(par._1)
      rm.setHyperParameters(par._2)
    }
    (wp, dr, ii, rm)
  }

  def evaluateModel(rm : RankingModel, verbose : Boolean = false): Double ={
    var MAP = 0.0
    for(queryId <- QueryMetric.codeToQuery.keys)
    {
      if (verbose ) logger.log ("=====================================================")
      val query = QueryMetric.codeToQuery(queryId)
      if (verbose ) println(query.split(' ').toList)
      val result = rm.query(query.split(' ').toList)
      printQueryDetails(queryId, result, 30)
      val metrics =  QueryMetric.eval(queryId, result.rankedDocs)
      MAP = MAP + metrics._4
      if(verbose) println( s"Query $queryId -> precision: ${metrics._1(100)}, recall: ${metrics._2(100)} , F1: ${metrics._3(100)}, AP: ${metrics._4}" )
    }
    MAP = MAP / QueryMetric.codeToQuery.size
    if (verbose) println(s"MAP: $MAP")
    MAP
  }


  def gridSearch() = {
    val modelType = readModelSetup()
    if (modelType.equals("tf"))
      gridSearchTf()
    else
      gridSearchLanguage()
  }


  def gridSearchTf(skipTuning : Boolean = false) = {
    var bestParams = ("lpn.nnn", 7.0)
    val (wp, dr, ii, rm) = setup("tf", bestParams)
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
            scores((docMode + "." + queryMode, fancyHitBonus)) = evaluateModel(rm)
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
    evaluateModel(rm, true)
  }

  def gridSearchLanguage(skipTuning : Boolean = false) = {
    var bestParams = (0.55,150,3.0)
    val (wp, dr, ii, rm) = setup("language", bestParams)
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
            scores((theta, zeta, fh)) = evaluateModel(rm)
          }
        }
      }
      logger.log("Best scores for each hyperparameters : ")
      scores.toList.sortBy(-_._2).foreach { x => logger.log(s"(theta ${x._1._1} , zeta ${x._1._2}, fancyHitBonus ${x._1._3}) => ${x._2}") }

      println("DETAILS FOR BEST HYPERPARAMETERS : ")
      bestParams = scores.toList.sortBy(-_._2).head._1
    }
    rm.setHyperParameters(bestParams._1, bestParams._2, bestParams._3)
    evaluateModel(rm, true)
  }


  def main(args: Array[String]): Unit ={
    val logger = new Logger("SearchEngine")
    //    evaluateTiming()

    if (!Files.exists(Paths.get(new File("./src/main/resources/documents.zip").getCanonicalPath)))
    {
      logger.log(s"${Console.RED} Ensure ${new File("./src/main/resources/documents.zip").getCanonicalPath} exists ${Console.RESET}")
      sys.exit(1)
    }
    if (!Files.exists(Paths.get(new File("./src/main/resources/questions-descriptions.txt").getCanonicalPath)))
    {
      logger.log(s"${Console.RED} Ensure ${new File("./src/main/resources/questions-descriptions.txt").getCanonicalPath} exists ${Console.RESET}")
      sys.exit(1)
    }
    if (!Files.exists(Paths.get(new File("./src/main/resources/relevance-judgements.csv").getCanonicalPath)))
    {
      logger.log(s"${Console.RED} Ensure ${new File("./src/main/resources/relevance-judgements.csv").getCanonicalPath} exists ${Console.RESET}")
      sys.exit(1)
    }

    if(args.length == 0)
    {
      logger.log("Got no args, running in manual mode")
      //TODO ensure that data is present before continuing
      println ("Starting SearchEngine. What do you want to run?")
      println ("=============================================================")
      println ("1.) Evaluate test queries on single model for hand in")
      println ("2.) Evaluate training queries and output metrics")
      println ("3.) Perform Timing comparison with and without inverted index")
      println ("4.) Run Grid search for parameters on test queries")
      println ("5.) Interactive search")
      println ("=============================================================")
      var i = 0
      do {
        println ("Your choice? [1-5]")
        i = scala.io.StdIn.readInt()
      } while (i < 1 || i > 5)
      i match {
        case 1 => sys.exit() //TODO implement me
        case 2 => evaluateTraining()
        case 3 => sys.exit() //TODO implement me
        case 4 => gridSearch()
        case 5 => interactive()
      }
      sys.exit()
    } else
    {
      logger.log("Got args")
      if ( !Set("tf","language").contains(args(0))){
        logger.log(s"${Console.RED} Please specify a valid model as first argument (either language or tf) ${Console.RESET}")
        return
      }
      var skipTuning = args.contains("--quick")

      if(args(0).equals("tf"))
        gridSearchTf(skipTuning)
      else
        gridSearchLanguage(skipTuning)

    }
  }
}