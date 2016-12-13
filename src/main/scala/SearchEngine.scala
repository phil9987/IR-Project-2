import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import scala.util.Try

/**
  * Start point for the program. Provides text user interface and starts the model accordingly.
  */
object SearchEngine {
  val logger = new Logger("SearchEngine")

  //the best found parameters / default parameters
  val bestTfFunctionString = "lnn.npn"
  val bestTfFHB = 10.0
  val bestLanguageTheta = .75
  val bestLanguageZeta = 200
  val bestLanguageFancyHitBonus = 3.0

  //methods for running the timing tests

  /**
    * Asks the user which timing test to use and runs it.
    */
  def timingTest(): Unit = {
    var timingType = ""
    do {
      println("Run without inverted index (pass-through) [pt], without  inverted index (pass-through) batch mode " +
                "[ptB], with inverted index [ii], with inverted index and levelDB [iiLDB]?")
      timingType = scala.io.StdIn.readLine()
    } while (!(timingType.equals("pt") || timingType.equals("ptB") || timingType.equals("ii") | timingType.equals
    ("iiLDB")))

    timingType match {
      case "pt" => timingTestPassThrough()
      case "ptB" => timingTestPassThroughBatching()
      case "ii" => timingTestInvertedIndex()
      case "iiLDB" => timingTestInvertedIndexLevelDB()
    }
  }

  /**
    * Run experiment to get the timing in pass-through mode with batching.
    * All queries are run in one batch and in order to answer the queries, then all files are traversed. No inverted
    * index is used.
    * Prints results to logger and writes them to a file.
    * Uses Language Model with best hyperparameters.
    */
  def timingTestPassThroughBatching(): Unit = {
    val filename = s"ptB_${java.time.LocalDateTime.now().toString}_runTimeMeasurement.txt"
    val t1 = System.nanoTime()
    val wp = new WordPreprocessor()
    val dr = new PassThroughDocumentReader(wp)
    val ii = new PassThroughInvertedIndex(dr)
    val t2 = System.nanoTime()
    val rm = new LanguageModel(ii, wp, bestLanguageTheta, bestLanguageZeta, bestLanguageFancyHitBonus)
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
    val queryTime = t3 - t2
    logger.log(s"Measured run time without inverted index in batch mode: setup $setupTime ns; query $queryTime ns; " +
                 s"MAP $MAP")
    val pw = new PrintWriter(new File(filename))
    pw.write(s"setup\t$setupTime\n")
    pw.write(s"query\t$queryTime\n")
    pw.write(s"MAP\t$MAP\n")
    pw.close()

  }

  /**
    * Run experiment to get the timing in pass-through mode.
    * All queries are run individually, and each time all files are traversed. No inverted index is used.
    * Prints results to logger and writes them to a file.
    * Uses Language Model with best hyperparameters.
    */
  def timingTestPassThrough(): Unit = {
    val filename = s"pt_${java.time.LocalDateTime.now().toString}_runTimeMeasurement.txt"
    val queryTimes = new collection.mutable.HashMap[Int, Long]
    val t1 = System.nanoTime()
    val wp = new WordPreprocessor()
    val dr = new PassThroughDocumentReader(wp)
    val ii = new PassThroughInvertedIndex(dr)
    val t2 = System.nanoTime()
    val rm = new LanguageModel(ii, wp, bestLanguageTheta, bestLanguageZeta, bestLanguageFancyHitBonus)
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
    val queryTime = t3 - t2
    logger.log(s"Measured run time without inverted index: setup $setupTime ns; query $queryTime ns; MAP $MAP")
    val pw = new PrintWriter(new File(filename))
    pw.write(s"setup\t$setupTime\n")
    pw.write(s"query\t$queryTime\n")
    pw.write(s"MAP\t$MAP\n")

    //print per query timings
    pw.write("\nquery\ttime\n")
    queryTimes.toList.sortBy(_._1).foreach { t => pw.write(s"${t._1}\t${t._2}\n") }
    pw.close()
  }

  /**
    * Run experiment to get the timing in inverted index mode. This uses the default inverted index.
    * All queries are run individually, and each is answered using the inverted index.
    * Prints results to logger and writes them to a file.
    * Uses Language Model with best hyperparameters.
    */
  def timingTestInvertedIndex(): Unit = {
    val filename = s"ii_${java.time.LocalDateTime.now().toString}_runTimeMeasurement.txt"
    val queryTimes = new collection.mutable.HashMap[Int, Long]
    val t1 = System.nanoTime()
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp)
    val ii = new InvertedIndex(dr)
    val t2 = System.nanoTime()
    val rm = new LanguageModel(ii, wp, bestLanguageTheta, bestLanguageZeta, bestLanguageFancyHitBonus)
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
    val queryTime = t3 - t2
    logger.log(s"Measured run time with inverted index: setup $setupTime ns; query $queryTime ns; MAP $MAP")
    val pw = new PrintWriter(new File(filename))
    pw.write(s"setup\t$setupTime\n")
    pw.write(s"query\t$queryTime\n")
    pw.write(s"MAP\t$MAP\n")

    //print per query timings
    pw.write("\nquery\ttime\n")
    queryTimes.toList.sortBy(_._1).foreach { t => pw.write(s"${t._1}\t${t._2}\n") }
    pw.close()
  }

  /**
    * Run experiment to get the timing in inverted index mode while using level-DB to store the index.
    * All queries are run individually, and each is answered using the inverted index.
    * Prints results to logger and writes them to a file.
    * Uses Language Model with best hyperparameters.
    */
  def timingTestInvertedIndexLevelDB(): Unit = {
    val filename = s"iiLDB_${java.time.LocalDateTime.now().toString}_runTimeMeasurement.txt"
    val queryTimes = new collection.mutable.HashMap[Int, Long]
    val t1 = System.nanoTime()
    val wp = new WordPreprocessor()
    val dr = new LevelDBDocumentReader(wp)
    val ii = new InvertedIndex(dr)
    val t2 = System.nanoTime()
    val rm = new LanguageModel(ii, wp, bestLanguageTheta, bestLanguageZeta, bestLanguageFancyHitBonus)
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
    val queryTime = t3 - t2
    logger.log(s"Measured run time with inverted index, stored in levelDB: setup $setupTime ns; query $queryTime ns; " +
                 s"MAP $MAP")
    val pw = new PrintWriter(new File(filename))
    pw.write(s"setup\t$setupTime\n")
    pw.write(s"query\t$queryTime\n")
    pw.write(s"MAP\t$MAP\n")

    //print per query timings
    pw.write("\nquery\ttime\n")
    queryTimes.toList.sortBy(_._1).foreach { t => pw.write(s"${t._1}\t${t._2}\n") }
    pw.close()
  }

  //Read user-input and create model accordingly

  /**
    * Asks the user whether to run the tf or the language model.
    *
    * @return "tf" or "language" accordingly.
    */
  def readModelSetup() = {
    var modelType = ""
    do {
      println("Run VectorSpace Model or Language Model? [tf, language]")
      modelType = scala.io.StdIn.readLine()
    } while (!(modelType.equals("tf") || modelType.equals("language")))
    modelType
  }

  /**
    * Asks the user which model to create and subsequently asks for the parameters.
    *
    * @return Either ("tf", (functionType, FancyHitBonus)) or ("language", (theta, zeta, fancyHitRange)).
    */
  def modelSetup() = {
    val modelType = readModelSetup()
    if (modelType.equals("tf"))
      readTfSetup()
    else
      readLanguageSetup()
  }

  /**
    * Asks the user for parameter details for the tf model.
    *
    * @return ("tf", (functionType, FancyHitBonus))
    */
  def readTfSetup() = {
    var functionTypes = ""
    do {
      println(s"Vector representation  in SMART notation( http://tinyurl.com/zgrqskx for details) : [ [nlb][ntp][nc]\\.[nlb][ntp][nc], default = $bestTfFunctionString ] ")
      functionTypes = scala.io.StdIn.readLine()
    } while (!(functionTypes.equals("") || functionTypes.matches("[nlpct]{3}\\.[nlpct]{3}")))
    if (functionTypes.equals("")) functionTypes = bestTfFunctionString
    var fancyHitBonus = ""
    do {
      println(s"Fancyhit bonus (counts as bonus occurrences if the word appears in the document title) [ double, default = $bestTfFHB ] ")
      fancyHitBonus = scala.io.StdIn.readLine()
    } while (!(fancyHitBonus.equals("") || Try {
                                                 fancyHitBonus.toDouble
                                               }.isSuccess))
    val fhb: Double = Try {
                            fancyHitBonus.toDouble
                          }.getOrElse(bestTfFHB)
    ("tf", (functionTypes, fhb))
  }

  /**
    * Asks the user for parameter details for the language model.
    *
    * @return ("language", (theta, zeta, fancyHitRange))
    */
  def readLanguageSetup() = {

    var thetaInput = ""
    do {
      println(s"theta : balances the document language model (more important when theta is close to 1) " +
        s"with the collection language model (creates smoothing, more important when theta is close to 0). " +
        s"[ double, default = $bestLanguageTheta ] ") //TODO write explanation
      thetaInput = scala.io.StdIn.readLine()
    } while (!(thetaInput.equals("") || Try {
                                              thetaInput.toDouble
                                            }.isSuccess))
    val theta: Double = Try {
                              thetaInput.toDouble
                            }.getOrElse(bestLanguageTheta)
    var zetaInput = ""
    do {
      println(s"zeta : constant to add to the document length (penalizes shorter documents) [ int, default = $bestLanguageZeta] ") //TODO write explanation
      zetaInput = scala.io.StdIn.readLine()
    } while (!(zetaInput.equals("") || Try {
                                             zetaInput.toInt
                                           }.isSuccess))
    val zeta: Int = Try {
                          zetaInput.toInt
                        }.getOrElse(bestLanguageZeta)
    var fhrInput = ""
    do {
      println(s"fancyHitBonus : ( counts as bonus occurrences if the word appears in the document title) [ double, default = $bestLanguageFancyHitBonus ] ") //TODO write explanation
      fhrInput = scala.io.StdIn.readLine()
    } while (!(fhrInput.equals("") || Try {
                                            fhrInput.toDouble
                                          }.isSuccess))
    val fhr: Double = Try {
                            fhrInput.toDouble
                          }.getOrElse(bestLanguageFancyHitBonus)
    ("language", (theta, zeta, fhr))
  }

  //Create the relevant setup given some configuration

  /**
    * Setup WordPreprocessor, DocumentReader, InvertedIndex and Ranking Model based on user input.
    *
    * @param modelType      "tf" or "language"
    * @param modelParameter Tuple with the model details. If "tf" is given this must be (functionType, FancyHitBonus)
    *                       and if "language" is given this must be (theta, zeta, fancyHitRange).
    * @return (WordPreprocessor, DocumentReader, InvertedIndex, RankingModel)
    */
  def setup(modelType: String,
            modelParameter: Serializable): (WordPreprocessor, DocumentReader, InvertedIndex, RankingModel)
  = {
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp)
    val ii = new InvertedIndex(dr)
    TermVectors.ii = ii
    val rm = if (modelType == "language") {
      val par = modelParameter.asInstanceOf[(Double, Int, Double)]
      new LanguageModel(ii, wp, par._1, par._2, par._3)
    } else {
      val par = modelParameter.asInstanceOf[(String, Double)]
      new VectorSpaceModel(ii, wp, par._1, par._2)
    }
    (wp, dr, ii, rm)
  }

  //Helper methods for different run time modes

  /**
    * Shows the details for a query from the training set. Displays relevant queries in green. Queries that are
    * explicitly irrelevant, but were retrieved by the training models in yellow and those for which the
    * ground-through is unknown in red.
    * Argument: The results are from other search engines. If they did not even retrieve the document, the document
    * must be quite bad for the query.
    *
    * @param queryId QueryId from the training set.
    * @param result  The result from the ranking model.
    * @param N       How many of the top documents tho show. Defaults to 30.
    */
  def printQueryDetails(queryId: Int, result: QueryResult, N: Int = 30, rm: RankingModel): Unit = {
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
      val numOfTokens = rm.ii.reader.documentInfo(result.docToWordMap(doc).head.docId)._1
      logger.log(s"$color TOP $i : $doc  ( $numOfTokens tokens) => ${
        result.docToWordMap(doc)
          .sortBy(_.word)
          .map(x
               => s"${x.word}: ${x.numOccurrence}").mkString(",")
      }")
      print(Console.RESET)
    }
  }

  /**
    * Given a setup (RankingModel, WordPreprocessor) runs the training queries on them, and reports detailed results
    * along with metrics.
    *
    * @param rm      The ranking model.
    * @param wp      The WordPreprocessor.
    * @param verbose Whether to print additional details. Defaults to false.
    * @return MAP score for the evaluation. (used in gridSearch mode)
    */
  def evaluateModel(rm: RankingModel, wp: WordPreprocessor, verbose: Boolean = false): Double = {
    var MAP = 0.0
    for (queryId <- QueryMetric.codeToQuery.keys) {
      if (verbose) logger.log("=====================================================")
      val query = wp.replaceImportantAbbreviations(QueryMetric.codeToQuery(queryId))
      if (verbose) println(query.split(' ').toList)
      val result = rm.query(query.split(' ').toList)
      if (verbose) printQueryDetails(queryId, result, 30, rm)
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


  //Different Modes of running the SearchEngine

  /**
    * Asks for user-input, starts according model and evaluates it on the training queries.
    */
  def evaluateTraining(): Unit = {
    val setupInfo = modelSetup()
    val (wp, dr, ii, rm) = setup(setupInfo._1, setupInfo._2)
    evaluateModel(rm, wp, verbose = true)
  }

  /**
    * Asks for user-input, starts according model and answers user input queries in real time.
    */
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

  /**
    * Asks the user for a model type and runs gridSearch for the parameters of that model type.
    */
  def gridSearch() = {
    val modelType = readModelSetup()
    if (modelType.equals("tf"))
      gridSearchTf()
    else
      gridSearchLanguage()
  }

  /**
    * Runs gridSearch for the parameters of the tf model.
    *
    * @param skipTuning Skip the actual gridSearch and use previous best. Defaults to false.
    */
  def gridSearchTf(skipTuning: Boolean = false) = {
    var bestParams = ("lpn.nnn", 7.0)
    val (wp, dr, ii, _) = setup("tf", bestParams)
    if (!skipTuning) {
      var count = 0
      val docsModesList = List("nnn", "nnc", "ntn", "ntc", "npn", "npc",
                               "lnn", "lnc", "ltn", "ltc", "lpn", "lpc",
                               "bnn", "bnc", "btn", "btc", "bpn", "bpc")
      var queryModesList = List("nnn", "npn","ntn")

      val fancyHitRange = List(0.0, 5.0, 7.0, 10.0, 15.0)

      val scores = scala.collection.mutable.Map[(String, Double), Double]()
      val totalCombinations = docsModesList.length * queryModesList.length * fancyHitRange.length
      for (docMode <- docsModesList) {
        for (queryMode <- queryModesList) {
          for (fancyHitBonus <- fancyHitRange) {
            count += 1
            logger.log(s"Tuning hyperparameters... ${100 * count / totalCombinations} percent done!")
            val rm = new VectorSpaceModel(ii, wp, docMode + "." + queryMode, fancyHitBonus)
            scores((docMode + "." + queryMode, fancyHitBonus)) = evaluateModel(rm, wp)
          }
        }
      }
      logger.log("Best scores for each hyperparameters : ")
      scores.toList.sortBy(-_._2).foreach { x => logger.log(s"${x._1} => ${x._2}") }

      println("DETAILS FOR BEST HYPERPARAMETERS : ")
      bestParams = scores.toList.sortBy(-_._2).head._1
    }
    val rm = new VectorSpaceModel(ii, wp, bestParams._1, bestParams._2)
    evaluateModel(rm, wp, verbose = true)
  }

  /**
    * Runs gridSearch for the parameters of the language model.
    *
    * @param skipTuning Skip the actual gridSearch and use previous best. Defaults to false.
    */
  def gridSearchLanguage(skipTuning: Boolean = false) = {
    var bestParams = (0.55, 150, 3.0)
    val (wp, dr, ii, _) = setup("language", bestParams)
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
            val rm = new LanguageModel(ii, wp, theta, zeta, fh)
            scores((theta, zeta, fh)) = evaluateModel(rm, wp)
          }
        }
      }
      logger.log("Best scores for each hyperparameters : ")
      scores.toList.sortBy(-_._2)
        .foreach { x => logger.log(s"(theta ${x._1._1} , zeta ${x._1._2}, fancyHitBonus ${x._1._3}) => ${x._2}") }

      println("DETAILS FOR BEST HYPERPARAMETERS : ")
      bestParams = scores.toList.sortBy(-_._2).head._1
    }
    val rm = new LanguageModel(ii, wp, bestParams._1, bestParams._2, bestParams._3)
    evaluateModel(rm, wp, verbose = true)
  }

  /**
    * Asks for user-input, starts according model and evaluates the test queries. The results are saved in an
    * appropriately named file.
    */
  def evaluateTest(): Unit = {
    val setupInfo = modelSetup()
    val (wp, dr, ii, rm) = setup(setupInfo._1, setupInfo._2)
    val pw = new PrintWriter(new File(s"ranking-${setupInfo._1.charAt(0)}-7.run"))
    for (queryId <- QueryReader.codeToQuery.keys.toList.sorted) {
      val result = rm.query(QueryReader.codeToQuery(queryId).split(' ').toList)
      result.rankedDocs.zipWithIndex.foreach { case (docName, rank) =>
        pw.write(s"$queryId ${rank + 1} $docName\n")
                                             }
    }
    pw.close()
    logger.log(s"wrote to ranking-${setupInfo._1.charAt(0)}-7.run")
  }

  /**
    * Start point of the program. Ensures all input files are present, and either asks the user for what to run or
    * takes command line parameters. Command line parameters will not be described for the end user and are mainly
    * used for internal testing.
    *
    * @param args The command line arguments.
    */
  def main(args: Array[String]): Unit = {
    val logger = new Logger("SearchEngine")

    //ensure all external files are given

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

    if (args.length == 0) {
      // show menu
      logger.log("Got no args, running in manual mode")
      println("Starting SearchEngine. What do you want to run?")
      println("=============================================================")
      println("1.) Evaluate test queries on single model for hand in. Generates files ranking-{l,t}-7.run")
      println("2.) Evaluate training queries and output metrics. Prompts for model and hyperparameters, runs model, prints score. ")
      println("3.) Run Grid search for parameters on training queries." +
        " Prints the MAP for each hyperparameter combination with details for the best solution.")
      println("4.) Perform Timing comparison with and without inverted index")
      println("5.) Interactive search")
      println("6.) Precompute Vector Norms. - Gets also executed once you use the tf model")
      println("=============================================================")
      var i = 0
      do {
        println("Your choice? [1-6]")
        i = scala.io.StdIn.readInt()
      } while (i < 1 || i > 6)
      i match {
        case 1 => evaluateTest()
        case 2 => evaluateTraining()
        case 3 => gridSearch()
        case 4 => timingTest()
        case 5 => interactive()
        case 6 => precomputeNorms()
      }
      sys.exit()
    } else {
      //start program with cli parameters
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
      else if (args(0).equals("time")) {
        if (args.length == 2) {
          args(1) match {
            case "pt" => timingTestPassThrough()
            case "ptB" => timingTestPassThroughBatching()
            case "ii" => timingTestInvertedIndex()
            case "iiLDB" => timingTestInvertedIndexLevelDB()
          }
        }
      }
    }
  }

  /**
    * Precomputes the norms used by the Vector Space model and stores them in the Database.
    */
  def precomputeNorms(): Unit = {
    val (wp, dr, ii, rm) = setup("language", ("nnn.nnn", 0.0)) //used to initialize inverted index
    TermVectors.saveNorms()
  }
}