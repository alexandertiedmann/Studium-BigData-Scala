package wordcount

import java.awt.{Color, GridLayout}

import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.ApplicationFrame
import org.jfree.util.ShapeUtilities


/**
  * @author hendrik
  * modified by akarakochev
  */
class Sentiments(sentiFile: String) {

  val sentiments: Map[String, Int] = getSentiments(sentiFile)

  val proc = new Processing()

  /** ********************************************************************************************
    *
    * Aufgabe 5
    *
    * ********************************************************************************************
    */

  def getDocumentGroupedByCounts(filename: String, wordCount: Int): List[(Int, List[String])] = {
    val wMap = Processing.getData(filename)
    wMap.flatMap(a => proc.getWords(a._2 )).grouped(wordCount).zipWithIndex.map{ // Erstelle eine neue Liste und gruppiere diese anhand
      // vorgegebenem WordCount und Index anhaengen
      l => (l._2+1, l._1) // Attribute noch umdrehen
    }.toList
  }

  def getDocumentSplitByPredicate(filename: String, predicate:String=>Boolean): List[(Int, List[String])] = {
    val pMap = Processing.getData(filename)
    //print(
    //pMap.filter(x => !predicate(x._2)).zipWithIndex.map(l => (l._2, l._1._2))
    //.toList)

    print(
      pMap.groupBy(x => !predicate(x._2))
      .toList
    )

    val wordCount = 1;
    pMap.flatMap(a => proc.getWords(a._2 )).grouped(wordCount).zipWithIndex.map{ // Erstelle eine neue Liste und gruppiere diese anhand
      // vorgegebenem WordCount und Index anhaengen
      l => (l._2+1, l._1) // Attribute noch umdrehen
    }.toList
  }

  def analyseSentiments(l: List[(Int, List[String])]): List[(Int, Double, Double)] = {
    l.foldLeft(List.empty[(Int,Double,Double)]){ // Erzeuge das Tripel - so geschrieben?
      (nList, tTuple) =>  (tTuple._1, (tTuple._2.map(sentiments.getOrElse(_,0)).sum.toDouble / tTuple._2.filter(f
        => sentiments.get(f).map{
          ele => ele >= -5 && ele <= 5
        }.getOrElse(false)).length),(tTuple._2.filter(p => sentiments.get(p).map{
          ele => ele >= -5 && ele <= 5
        }.getOrElse(false)).length.toDouble / tTuple._2.length)) :: nList
    }.reverse
    // Erstes Element ist der Absatz, zweites Element ist das Auslesen der Map mit den Woertern, um Sentimentwert zu bekommen
    // Wenn das Wort nicht in der Map ist, einfach +0 addieren, aendert nix am Wert
    // Drittes Element zaehlt, wieviele Woerter in der Map gefunden wurden und teilt das durch die Gesamtzahl der Woerter aus der Quelle
  }

  /** ********************************************************************************************
    *
    * Helper Functions
    *
    * ********************************************************************************************
    */

  def getSentiments(filename: String): Map[String, Int] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    val result: Map[String, Int] = (for (row <- iter) yield {
      val seg = row.split("\t"); (seg(0) -> seg(1).toInt)
    }).toMap
    src.close()
    result
  }

  def createGraph(data: List[(Int, Double, Double)], xlabel:String="Abschnitt", title:String="Sentiment-Analyse"): Unit = {

    //create xy series
    val sentimentsSeries: XYSeries = new XYSeries("Sentiment-Werte")
    data.foreach { case (i, sentimentValue, _) => sentimentsSeries.add(i, sentimentValue) }
    val relWordsSeries: XYSeries = new XYSeries("Relative Haeufigkeit der erkannten Worte")
    data.foreach { case (i, _, relWordsValue) => relWordsSeries.add(i, relWordsValue) }

    //create xy collections
    val sentimentsDataset: XYSeriesCollection = new XYSeriesCollection()
    sentimentsDataset.addSeries(sentimentsSeries)
    val relWordsDataset: XYSeriesCollection = new XYSeriesCollection()
    relWordsDataset.addSeries(relWordsSeries)

    //create renderers
    val relWordsDot: XYDotRenderer = new XYDotRenderer()
    relWordsDot.setDotHeight(5)
    relWordsDot.setDotWidth(5)
    relWordsDot.setSeriesShape(0, ShapeUtilities.createDiagonalCross(3, 1))
    relWordsDot.setSeriesPaint(0, Color.BLUE)

    val sentimentsDot: XYDotRenderer = new XYDotRenderer()
    sentimentsDot.setDotHeight(5)
    sentimentsDot.setDotWidth(5)

    //create xy axis
    val xax: NumberAxis = new NumberAxis(xlabel)
    val y1ax: NumberAxis = new NumberAxis("Sentiment Werte")
    val y2ax: NumberAxis = new NumberAxis("Relative Haeufigfkeit")

    //create plots
    val plot1: XYPlot = new XYPlot(sentimentsDataset, xax, y1ax, sentimentsDot)
    val plot2: XYPlot = new XYPlot(relWordsDataset, xax, y2ax, relWordsDot)

    val chart1: JFreeChart = new JFreeChart(plot1)
    val chart2: JFreeChart = new JFreeChart(plot2)
    val frame: ApplicationFrame = new ApplicationFrame(title)
    frame.setLayout(new GridLayout(2,1))

    val chartPanel1: ChartPanel = new ChartPanel(chart1)
    val chartPanel2: ChartPanel = new ChartPanel(chart2)

    frame.add(chartPanel1)
    frame.add(chartPanel2)
    frame.pack()
    frame.setVisible(true)
  }
}
