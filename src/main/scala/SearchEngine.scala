import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import scala.util.Try

object SearchEngine {
  val logger = new Logger("SearchEngine")


  val bestTfFunctionString = "lpn.nnn"
  val bestTfFHB = 7.0
  val bestLanguageTheta = .75
  val bestLanguageZeta = 200
  val bestLanguageFancyHitRange = 3.0

  def evaluatePassThroughBatching() = {
    //Test: PassThrough Inverted Index + Batching
    val filename = s"ptB_${java.time.LocalDateTime.now().toString}_runTimeMeasurement.txt"
    val t1 = System.nanoTime()
    val wp = new WordPreprocessor()
    val dr = new PassThroughDocumentReader(wp, 1000)
    val ii = new PassThroughInvertedIndex(dr)
    val t2 = System.nanoTime()
    val rm = new LanguageModel(ii, wp)
    rm.setHyperParameters(bestLanguageTheta,bestLanguageZeta, bestLanguageFancyHitRange)
    var MAP = 0.0
    val keys = QueryMetric.codeToQuery.keys.toList
    for (queryId <- keys) {
      val query = QueryMetric.codeToQuery(queryId)
      rm.enqueueBatchQuery(query.split(' ').toList)
    }
    val results = rm.performBatchQuery()
    for (i <- keys.indices) {
      val ranking = results(i).rankedDocs
      val queryId = keys(i)
      val metrics = QueryMetric.eval(queryId, ranking)
      MAP = MAP + metrics._4
    }
    val t3 = System.nanoTime()
    MAP = MAP / QueryMetric.codeToQuery.size
    val setupTime = t2 - t1
    val queryTime = t3 -t2
    logger.log(s"Without inverted Index, batch mode: setup $setupTime; query $queryTime; MAP $MAP")
    val pw = new PrintWriter(new File(filename))
    pw.write(s"setup\t$setupTime\n")
    pw.write(s"query\t$queryTime\n")
    pw.write(s"MAP\t$MAP\n")
    pw.close()

  }

  def evaluatePassThrough() = {
    val filename = s"pt_${java.time.LocalDateTime.now().toString}_runTimeMeasurement.txt"
    val queryTimes = new collection.mutable.HashMap[Int, Long]
    val t1 = System.nanoTime()
    val wp = new WordPreprocessor()
    val dr = new PassThroughDocumentReader(wp)
    val ii = new PassThroughInvertedIndex(dr)
    val t2 = System.nanoTime()
    val rm = new LanguageModel(ii, wp)
    rm.setHyperParameters(bestLanguageTheta,bestLanguageZeta, bestLanguageFancyHitRange)
    var MAP = 0.0
    val keys = QueryMetric.codeToQuery.keys.toList
    for (queryId <- keys) {
      val query = QueryMetric.codeToQuery(queryId)
      val tq1 = System.nanoTime()
      val result = rm.query(query.split(' ').toList)
      val tq2 = System.nanoTime()
      queryTimes(queryId) = tq2 - tq1
      val metrics = QueryMetric.eval(queryId, result.rankedDocs)
      MAP = MAP + metrics._4
    }
    val t3 = System.nanoTime()
    MAP = MAP / QueryMetric.codeToQuery.size
    val setupTime = t2 - t1
    val queryTime = t3 -t2
    logger.log(s"Without inverted Index: setup $setupTime; query $queryTime; MAP $MAP")
    val pw = new PrintWriter(new File(filename))
    pw.write(s"setup\t$setupTime\n")
    pw.write(s"query\t$queryTime\n")
    pw.write(s"MAP\t$MAP\n")

    pw.write("\nquery\ttime\n")
    queryTimes.toList.sortBy(_._1).foreach{t => pw.write(s"${t._1}\t${t._2}\n")}
    pw.close()
  }

  def evaluateInvertedIndex() = {
    val filename = s"ii_${java.time.LocalDateTime.now().toString}_runTimeMeasurement.txt"
    val queryTimes = new collection.mutable.HashMap[Int, Long]
    val t1 = System.nanoTime()
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp)
    val ii = new InvertedIndex(dr)
    val t2 = System.nanoTime()
    val rm = new LanguageModel(ii, wp)
    rm.setHyperParameters(bestLanguageTheta,bestLanguageZeta, bestLanguageFancyHitRange)
    var MAP = 0.0
    val keys = QueryMetric.codeToQuery.keys.toList
    for (queryId <- keys) {
      val query = QueryMetric.codeToQuery(queryId)
      val tq1 = System.nanoTime()
      val result = rm.query(query.split(' ').toList)
      val tq2 = System.nanoTime()
      queryTimes(queryId) = tq2 - tq1
      val metrics = QueryMetric.eval(queryId, result.rankedDocs)
      MAP = MAP + metrics._4
    }
    val t3 = System.nanoTime()
    MAP = MAP / QueryMetric.codeToQuery.size
    val setupTime = t2 - t1
    val queryTime = t3 -t2
    logger.log(s"With inverted Index: setup $setupTime; query $queryTime; MAP $MAP")
    val pw = new PrintWriter(new File(filename))
    pw.write(s"setup\t$setupTime\n")
    pw.write(s"query\t$queryTime\n")
    pw.write(s"MAP\t$MAP\n")

    pw.write("\nquery\ttime\n")
    queryTimes.toList.sortBy(_._1).foreach{t => pw.write(s"${t._1}\t${t._2}\n")}
    pw.close()
  }


  def evaluateInvertedIndexLevelDB() = {
    val filename = s"iiLDB_${java.time.LocalDateTime.now().toString}_runTimeMeasurement.txt"
    val queryTimes = new collection.mutable.HashMap[Int, Long]
    val t1 = System.nanoTime()
    val wp = new WordPreprocessor()
    val dr = new LevelDBDocumentReader(wp)
    val ii = new InvertedIndex(dr)
    val t2 = System.nanoTime()
    val rm = new LanguageModel(ii, wp)
    rm.setHyperParameters(bestLanguageTheta,bestLanguageZeta, bestLanguageFancyHitRange)
    var MAP = 0.0
    val keys = QueryMetric.codeToQuery.keys.toList
    for (queryId <- keys) {
      val query = QueryMetric.codeToQuery(queryId)
      val tq1 = System.nanoTime()
      val result = rm.query(query.split(' ').toList)
      val tq2 = System.nanoTime()
      queryTimes(queryId) = tq2 - tq1
      val metrics = QueryMetric.eval(queryId, result.rankedDocs)
      MAP = MAP + metrics._4
    }
    val t3 = System.nanoTime()
    MAP = MAP / QueryMetric.codeToQuery.size
    val setupTime = t2 - t1
    val queryTime = t3 -t2
    logger.log(s"With inverted Index with levelDB: setup $setupTime; query $queryTime; MAP $MAP")
    val pw = new PrintWriter(new File(filename))
    pw.write(s"setup\t$setupTime\n")
    pw.write(s"query\t$queryTime\n")
    pw.write(s"MAP\t$MAP\n")

    pw.write("\nquery\ttime\n")
    queryTimes.toList.sortBy(_._1).foreach{t => pw.write(s"${t._1}\t${t._2}\n")}
    pw.close()
  }

  def evaluateTiming(): Unit = {

    var timingType = ""
    do {
      println("Run without inverted index (pass-through) [pt], without  inverted index (pass-through) batch mode " +
                "[ptB], with inverted index [ii], with inverted index and levelDB [iiLDB]?")
      timingType = scala.io.StdIn.readLine()
    } while (!(timingType.equals("pt") || timingType.equals("ptB") || timingType.equals("ii") | timingType.equals
    ("iiLDB")))

    timingType match {
      case "pt" => evaluatePassThrough()
      case "ptB" => evaluatePassThroughBatching()
      case "ii" => evaluateInvertedIndex()
      case "iiLDB" => evaluateInvertedIndexLevelDB()
    }

  }


  def printQueryDetails(queryId: Int, result: QueryResult, N: Int = 30): Unit = {
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
      logger.log(s"$color TOP $i : $doc  tokens) => ${
        result.docToWordMap(doc)
          .sortBy(_.word)
          .map(x
          => s"${x.word}: ${x.numOccurrence}").mkString(",")
      }")
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
      println(s"TODO description [ [nlp]{r}\\.[nlp]{3}, default = $bestTfFunctionString ] ") //TODO write explanation
      functionTypes = scala.io.StdIn.readLine()
    } while (!(functionTypes.equals("") || functionTypes.matches("[nlp]{3}\\.[nlp]{3}")))
    if (functionTypes.equals("")) functionTypes = bestTfFunctionString
    var fancyHitBonus = ""
    do {
      println(s"TODO description fancy hit bonus [ double, default = $bestTfFHB ] ") //TODO write explanation
      fancyHitBonus = scala.io.StdIn.readLine()
    } while (!(fancyHitBonus.equals("") || Try {
      fancyHitBonus.toDouble
    }.isSuccess))
    val fhb: Double = Try {
      fancyHitBonus.toDouble
    }.getOrElse(bestTfFHB)
    ("tf", (functionTypes, fhb))
  }

  def readLanguageSetup() = {

    var thetaInput = ""
    do {
      println(s"TODO theta [ double, default = $bestLanguageTheta ] ") //TODO write explanation
      thetaInput = scala.io.StdIn.readLine()
    } while (!(thetaInput.equals("") || Try {
      thetaInput.toDouble
    }.isSuccess))
    val theta: Double = Try {
      thetaInput.toDouble
    }.getOrElse(bestLanguageTheta)
    var zetaInput = ""
    do {
      println(s"TODO zeta [ int, default = $bestLanguageZeta] ") //TODO write explanation
      zetaInput = scala.io.StdIn.readLine()
    } while (!(zetaInput.equals("") || Try {
      zetaInput.toInt
    }.isSuccess))
    val zeta: Int = Try {
      zetaInput.toInt
    }.getOrElse(bestLanguageZeta)
    var fhrInput = ""
    do {
      println(s"TODO fhr [ double, default = $bestLanguageFancyHitRange ] ") //TODO write explanation
      fhrInput = scala.io.StdIn.readLine()
    } while (!(fhrInput.equals("") || Try {
      fhrInput.toDouble
    }.isSuccess))
    val fhr: Double = Try {
      fhrInput.toDouble
    }.getOrElse(bestLanguageFancyHitRange)
    ("language", (theta, zeta, fhr))
  }

  def evaluateTraining(): Unit = {
    val setupInfo = modelSetup()
    val (wp, dr, ii, rm) = setup(setupInfo._1, setupInfo._2)
    evaluateModel(rm, verbose = true)
  }

  def interactive(): Unit = {
    val setupInfo = modelSetup()
    val (wp, dr, ii, rm) = setup(setupInfo._1, setupInfo._2)
    var query = ""
    while (true) {
      print("Enter query: ")
      query = scala.io.StdIn.readLine()
      println(rm.query(query.split(' ').toList).rankedDocs.mkString("[", ", ", "]"))
    }
  }

  def setup(modelType: String,
            modelParameter: Serializable): (WordPreprocessor, DocumentReader, InvertedIndex, RankingModel)
  = {
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp)
    val ii = new InvertedIndex(dr)
    val rm = if (modelType == "language") {
      new LanguageModel(ii, wp)
    } else {
      new VectorSpaceModel(ii, wp)
    }
    if (modelType == "language") {
      val par = modelParameter.asInstanceOf[(Double, Int, Double)]
      rm.setHyperParameters(par._1, par._2, par._3)
    } else {
      val par = modelParameter.asInstanceOf[(String, Double)]
      rm.setModelMode(par._1)
      rm.setHyperParameters(par._2)
    }
    (wp, dr, ii, rm)
  }

  def evaluateModel(rm: RankingModel, verbose: Boolean = false): Double = {
    var MAP = 0.0
    var wp = new WordPreprocessor()
    for (queryId <- QueryMetric.codeToQuery.keys) {
      if (verbose) logger.log("=====================================================")
      val query = wp.replaceImportantAbbreviations(QueryMetric.codeToQuery(queryId))
      if (verbose) println(query.split(' ').toList)
      val result = rm.query(query.split(' ').toList)
      printQueryDetails(queryId, result, 30)
      val metrics = QueryMetric.eval(queryId, result.rankedDocs)
      MAP = MAP + metrics._4
      if (verbose) println(s"Query $queryId -> precision: ${metrics._1(100)}, recall: ${metrics._2(100)} , F1: ${
        metrics._3(100)
      }, AP: ${metrics._4}")
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


  def gridSearchTf(skipTuning: Boolean = false) = {
    var bestParams = ("lpn.nnn", 7.0)
    val (wp, dr, ii, rm) = setup("tf", bestParams)
    if (!skipTuning) {
      var count = 0
      val docsModesList = List("nnn", "nnc", "ntn", "ntc", "npn", "npc",
        "lnn", "lnc", "ltn", "ltc", "lpn", "lpc",
        "bnn", "bnc", "btn", "btc", "bpn", "bpc")

      val queryModesList = List("nnn")
      val fancyHitRange = List(0.0, 5.0, 7.0, 10.0, 15.0)

      val scores = scala.collection.mutable.Map[(String, Double), Double]()
      val totalCombinations = docsModesList.length * queryModesList.length * fancyHitRange.length
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
    evaluateModel(rm, verbose = true)
  }

  def gridSearchLanguage(skipTuning: Boolean = false) = {
    var bestParams = (0.55, 150, 3.0)
    val (wp, dr, ii, rm) = setup("language", bestParams)
    if (!skipTuning) {
      val thetaRange = List(0.55, 0.60, 0.65, 0.7, 0.75, 0.8)
      val zetaRange = List(100, 150, 200, 250, 300)
      val fancyHitRange = List(0.0, 1.0, 3.0, 5.0, 7.0)
      val totalCombinations = thetaRange.length * zetaRange.length * fancyHitRange.length
      var count = 0

      val scores = scala.collection.mutable.Map[(Double, Int, Double), Double]()
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
      scores.toList.sortBy(-_._2)
        .foreach { x => logger.log(s"(theta ${x._1._1} , zeta ${x._1._2}, fancyHitBonus ${x._1._3}) => ${x._2}") }

      println("DETAILS FOR BEST HYPERPARAMETERS : ")
      bestParams = scores.toList.sortBy(-_._2).head._1
    }
    rm.setHyperParameters(bestParams._1, bestParams._2, bestParams._3)
    evaluateModel(rm, verbose = true)
  }


  def main(args: Array[String]): Unit = {
    val logger = new Logger("SearchEngine")
    //    evaluateTiming()

    if (!Files.exists(Paths.get(new File("./src/main/resources/documents.zip").getCanonicalPath))) {
      logger.log(s"${Console.RED} Ensure ${new File("./src/main/resources/documents.zip").getCanonicalPath} exists ${
        Console.RESET
      }")
      sys.exit(1)
    }
    if (!Files.exists(Paths.get(new File("./src/main/resources/questions-descriptions.txt").getCanonicalPath))) {
      logger.log(s"${Console.RED} Ensure ${
        new File("./src/main/resources/questions-descriptions.txt").getCanonicalPath
      } exists ${Console.RESET}")
      sys.exit(1)
    }
    if (!Files.exists(Paths.get(new File("./src/main/resources/relevance-judgements.csv").getCanonicalPath))) {
      logger.log(s"${Console.RED} Ensure ${
        new File("./src/main/resources/relevance-judgements.csv").getCanonicalPath
      } exists ${Console.RESET}")
      sys.exit(1)
    }
    if (!Files.exists(Paths.get(new File("./src/main/resources/test-questions.txt").getCanonicalPath))) {
      logger.log(s"${Console.RED} Ensure ${
        new File("./src/main/resources/test-questions.txt").getCanonicalPath
      } exists ${Console.RESET}")
      sys.exit(1)
    }


    def evaluateTest(): Unit = {
      val setupInfo = modelSetup()
      val (wp, dr, ii, rm) = setup(setupInfo._1, setupInfo._2)
      for (queryId <- QueryReader.codeToQuery.keys) {
        val result = rm.query(QueryReader.codeToQuery(queryId).split(' ').toList)
        val pw = new PrintWriter(new File(s"ranking-${setupInfo._1.charAt(0)}-7.run" ))
        result.rankedDocs.zipWithIndex.foreach{ case (docName, rank) =>
          pw.write(s"$queryId ${rank + 1} $docName\n")
        }
        pw.close()
      }
      logger.log(s"wrote to ranking-${setupInfo._1.charAt(0)}-7.run")
    }


    if (args.length == 0) {
      logger.log("Got no args, running in manual mode")
      println("Starting SearchEngine. What do you want to run?")
      println("=============================================================")
      println("1.) Evaluate test queries on single model for hand in")
      println("2.) Evaluate training queries and output metrics")
      println("3.) Perform Timing comparison with and without inverted index")
      println("4.) Run Grid search for parameters on test queries")
      println("5.) Interactive search")
      println("=============================================================")
      var i = 0
      do {
        println("Your choice? [1-5]")
        i = scala.io.StdIn.readInt()
      } while (i < 1 || i > 5)
      i match {
        case 1 => evaluateTest()
        case 2 => evaluateTraining()
        case 3 => evaluateTiming()
        case 4 => gridSearch()
        case 5 => interactive()
      }
      sys.exit()
    } else {
      logger.log("Got args")
      if (!Set("tf", "language", "time").contains(args(0))) {
        logger.log(s"${Console.RED} Please specify a valid model as first argument (either language or tf) or time " +
                     s"for timing mode${
                       Console.RESET
                     }")
        return
      }
      val skipTuning = args.contains("--quick")

      if (args(0).equals("tf"))
        gridSearchTf(skipTuning)
      else if (args(0).equals("language"))
        gridSearchLanguage(skipTuning)
      else if (args(0).equals("time"))
    {
      if (args.length == 2)
        {
          args(1) match {
            case "pt" => evaluatePassThrough()
            case "ptB" => evaluatePassThroughBatching()
            case "ii" => evaluateInvertedIndex()
            case "iiLDB" => evaluateInvertedIndexLevelDB()
          }
        }
    }

    }
  }
}