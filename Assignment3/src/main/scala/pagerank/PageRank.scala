package pagerank

import org.apache.spark.rdd.RDD
import pagerank.models.Page

import scala.annotation.tailrec

object PageRank {

  /**
    * Computes the pageRank based on the given links.
    *
    * @param links the links
    * @param t     teleportation
    * @param delta minimum value for difference
    * @return pageRanks
    **/
  def computePageRank(links: RDD[(String, Set[String])], t: Double = 0.15, delta: Double = 0.01): RDD[(String, Double)] = {
    val n = links.count()
    val tNorm = t / n
    val ranks = links.mapValues(_ => 1.0 / n)

    @tailrec
    def inner(ranks: RDD[(String, Double)], links: RDD[(String, Set[String])], tNorm: Double, t: Double, delta: Double)
    : RDD[(String, Double)] = {

      //compute the contributions
      val contributions = computeContributions(ranks, links)

      //combine same keys and apply teleportation and damping factors after
      val newRanks = computeNewRanksFromContributions(contributions, tNorm, t)

      //print(newRanks.collect().toList)

      val diff = computeDifference(ranks, newRanks)
      //done, difference is small enough
      if (diff < delta)
        newRanks
      else
        inner(newRanks, links, tNorm, t, delta)
    }

    inner(ranks, links, tNorm, t, delta)
  }

  /**
    * Computes the contributions from the given ranks and links.
    *
    * See the tests and the description in the assignment sheet for more information.
    *
    *
    * HINTs:
    * join and flatMap might be useful
    *
    * - What happens if a page is never linked to? ex. {A->B, B->B}
    * make sure that (A,0) is also in the result, so A doesn't get lost
    *
    * - also pay attention that A->{} should be treated as A->{A} and contribute (A,1)
    *
    */
  def computeContributions(ranks: RDD[(String, Double)], links: RDD[(String, Set[String])]): RDD[(String, Double)] = {
    // wenn Set leer -> fuellen mit eigener Seite
    /*val newLinks:RDD[(String, Set[String])] = links.map( l =>
      if (l._2.isEmpty) (l._1,Set(l._1))
      else (l._1,l._2)
    )*/
    val contributions = ranks.
      join(links).
      flatMap {
        //(A,(B,C))  -> x._2._1
        case (k, (rank, urls)) =>
          val n = urls.size.toDouble
          val m = urls.map(url => url -> rank / n).toMap
          if (m.isEmpty)
          //if no links, 1
            Map(k -> rank * 1.0)
          else
          //if nothing links, still in result
            m.updated(k, m.getOrElse(k, 0.0))
      }
    contributions
  }

  /**
    *
    * Computes the new ranks from the contributions
    * The difference is computed the following way in pseudocode:
    *
    * foreach key:
    * - sum its values
    * multiply the values obtained in the previous step by (1-t) and add tNorm
    *
    **/
  def computeNewRanksFromContributions(contributions: RDD[(String, Double)], tNorm: Double, t: Double): RDD[(String, Double)] = {
    val onet = 1 - t
    val newCon = contributions.reduceByKey((a,b) => a + b) //zusammenfuehren der gleichen Links und Summieren der Cons
    newCon.map(x => (x._1,tNorm+onet*x._2)) //Berechnung der Ranks
  }

  /**
    *
    * Computes the difference between the old and new ranks.
    * The difference is computed the following way in pseudocode:
    *
    * foreach key:
    * - obtain the absolute value/modulus of its value from ranks subtracted its value in newRanks
    * sum the values
    *
    **/
  def computeDifference(ranks: RDD[(String, Double)], newRanks: RDD[(String, Double)]): Double = {
    val joined = ranks.join(newRanks) //zusammenfuehren der beiden RDDs
      .map(s => (s._1,s._2._2 - s._2._1))  //Subtrahieren der alten von den neuen Ranks
    val values = joined.map(s => s._2) //extrahieren der Unterschiede
    values.sum() //summieren der Unterschiede
  }

  /**
    * Extracts all links from the given page RDD. This is a 3 step process:
    *
    * 1. Project the pages in the form of Title -> Set(links.titles)
    * 2. Project all other links in the form link.title -> Set()
    * 3. Merge the 2 using the rdd union operation followed by a reduceByKey
    *
    * This results in all pages, who have links to be a pair of pageTitle -> Set (linkTitle, linkTitle..)
    * and all pages, who don't have any links in the form of pageTitle -> Set()
    *
    * For some examples see the test cases
    */
  def extractLinksFromPages(pages: RDD[Page]): RDD[(String, Set[String])] = {
    //step 1
    pages.foreach(x => print("before " + x + " \n"))
    val extract1 = pages
      .map(p => (p.title, //titel der Seite nehmen
        p.links.map(x => x.title).toSet)) //links der Seite nehmen
    extract1.foreach(x => print("extract1 " + x + "\n"))
    //step 2
    val extract2 = pages
      .map(p => p.links.map(x => x))  //extrahieren der link-Listen
      .flatMap(x => x.map(x => x.title)) //extrahieren der einzelnen Seiten aus den Listen
      .map(x => (x,Set.empty[String])) //in endform giessen
    extract2.foreach(s => print("extract2 " + s + "\n"))
    //step 3
    val extracted = extract1.union(extract2) //zusammenfuehren
    extracted.foreach(s => print("final " + s + "\n"))

    //step 4
    //nimm die Werte des ersten gefundenen Eintrags (alle anderen aus Schritt 2 sind eh leer)
    val finalExtract = extracted.reduceByKey((a,b) => a)
    finalExtract
  }

}