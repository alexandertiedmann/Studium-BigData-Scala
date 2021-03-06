package twitter

import org.apache.spark.rdd.RDD
import twitter.models.Tweet

/**
  * @param tData twitter data rdd
  */
class TwitterAnalyzer(tData: RDD[Tweet]) {

  /*
   * Write a function that counts the number of tweets using the german language
   */
  def getGermanTweetsCount: Long = {
   tData.filter(t => t.lang.equals("de"))
     .count()
  }

  /*
   * Extracts the texts of all german tweets (all tweets with the "de" locale)
   */
  def getGermanTweetTexts: Array[String] = {
    tData.filter(t => t.lang.equals("de"))
      .map(t => t.text)
      .collect()
  }

  /**
    * Counts the number of unique german users (all users that tweeted using the "de" locale)
    */
  def numberOfGermanTweetsPerUser: Array[(String, Int)] = {
    tData.filter(t => t.lang.equals("de"))
      .map(t => t.userName)
      .countByValue()
      .map(f => (f._1,f._2.toInt))
      .toArray
  }

  /**
    * Counts the number of tweets per country
    */
  def numberOfTweetsPerCountry: Array[(String, Int)] = {
    tData.map(t => t.lang)
      .countByValue()
      .map(f => (f._1,f._2.toInt))
      .toArray
  }

  /**
    * Extracts the top 10 hashtags in english tweets (tweets with the "en" locale).
    *
    * Hints:
    * Use the [[TwitterAnalyzer.getHashtags()]] function to obtain the hashtags from the text of a tweet.
    * Checkout RDD.takeOrdered
    * https://spark.apache.org/docs/latest/api/scala/index.html#org.apache.spark.rdd.RDD@takeOrdered(num:Int)(implicitord:Ordering[T]):Array[T]
    * or RDD.top
    * https://spark.apache.org/docs/latest/api/scala/index.html#org.apache.spark.rdd.RDD@top(num:Int)(implicitord:Ordering[T]):Array[T]
    * and Ordering.by
    */
  def getTopTenEnglishHashtags: List[(String, Int)] = {
    tData.filter(t => t.lang.equals("en"))  //englische herausfiltern
      .map(t => t.text) //Die Texte in einen String
      .map(s => TwitterAnalyzer.getHashtags(s)) //den gesamten Wortschatz in die getHashtags Methode
      .filter(l => l.nonEmpty)  // nach nicht leeren filtern
      .toLocalIterator.toList.flatten //Die RDD[List()] in List[String] umwandeln //am Ende erst
      .groupBy(identity)  // nach Wort gruppieren
      .mapValues(_.size)  // zaehlen und unique
      .toList             //wieder in eine Liste
      .sortWith(_._2 > _._2)  //Liste high to low sortieren
      .take(10)   //hoechsten 10 eintraaege herausnehmen
  }
}

object TwitterAnalyzer {

  def getHashtags(text: String): List[String] = {
    if (text.isEmpty || text.length == 1) List()
    else if (text.head == '#') {
      val tag = text.takeWhile(x => x != ' ')
      val rest = text.drop(tag.length)
      tag :: getHashtags(rest)
    }
    else getHashtags(text.tail)
  }
}