object SearchEngine{

  def main(args: Array[String]): Unit ={
    val wp = new WordPreprocessor()
    val dr = new DocumentReader(wp)
    val ii = new DefaultInvertedIndex(dr)
    val rm = new DefaultRankingModel(ii, wp)

    rm.query("the cat jumps asdf elephant".split(' ').toList)

  }
}