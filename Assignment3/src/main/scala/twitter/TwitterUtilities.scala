package twitter

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

import twitter.models.Tweet
import utils.JsonUtils

object TwitterUtilities {
  val dtf: DateTimeFormatter = DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss X uuuu", Locale.ENGLISH)

  /*
  {
  "created_at": "Thu Apr 06 15:24:15 +0000 2017",
  "id_str": "850006245121695744",
  "text": "1\/ Today we\u2019re sharing our vision for the future of the Twitter API platform!\nhttps:\/\/t.co\/XweGngmxlP",
  "user": {
    "id": 2244994945,
    "name": "Twitter Dev",
    "screen_name": "TwitterDev",
    "location": "Internet",
    "url": "https:\/\/dev.twitter.com\/",
    "description": "Your official source for Twitter Platform news, updates & events. Need technical help? Visit https:\/\/twittercommunity.com\/ \u2328\ufe0f #TapIntoTwitter"
  },
  "place": {   
  },
  "entities": {
    "hashtags": [      
    ],
    "urls": [
      {
        "url": "https:\/\/t.co\/XweGngmxlP",
        "unwound": {
          "url": "https:\/\/cards.twitter.com\/cards\/18ce53wgo4h\/3xo1c",
          "title": "Building the Future of the Twitter API Platform"
        }
      }
    ],
    "user_mentions": [
    ]
  },
  "lang":"en"
}
*/

  /**
    * Parses a given tweet in the Twitter Data JSON Format using [[JsonUtils.parseJson()]]
    * and extracts the date, username, text and language of the tweet into a [[Tweet]] object.
    * The [[TwitterUtilities.getTwitterDate()]] function is used to parse the date.
    * If the line is not a valid json string, None is returned instead.
    *
    * Hints:
    * Analyse the Twitter Data Format (an entry is pasted above for convenience).
    * The date corresponds to the "created_at" field, the userName to the "name" field
    * and the text and language to the "text" and "lang" fields of the json string.
    *
    * Pay attention, that not all lines are actual tweets and don't comply to the above format.
    * (Checkout the tweets.txt File in the test resources)
    * Handle this by returning None
    *
    * If you are not too familiar with scala's Option class, checkout
    * https://www.scala-lang.org/api/2.12.x/scala/Option.html
    * and
    * https://www.tutorialspoint.com/scala/scala_options.htm
    *
    * If you are having trouble with casting checkout
    * https://alvinalexander.com/scala/how-to-cast-objects-class-instance-in-scala-asinstanceof
    */
  def parse(jsonString: String): Option[Tweet] = {
    def getUser(x: Map[String, Any]): String ={
      //Auslesen des Users (Map)
      var userNameString = x.get("user").getOrElse("user", "")
      var userName:String =
        try {
          var userName = userNameString.asInstanceOf[Map[String, String]] //aus Some eine Map machen
              .getOrElse("name", "") //aus der Map den Namen ziehen
            //println(userName)
            userName
        }catch{
          case e: Exception => {
            //println("String to Instance failed: " + userNameString)
            ""
          }
      }
      userName
    }
    def getdate(x:Map[String, Any]): OffsetDateTime={
      val stringDate = x.get("created_at") //Some aus JSON ziehen
        .getOrElse("created_at", "").toString //String aus Some machen
      val date = getTwitterDate(stringDate)
      if (getTwitterDate("").equals(date)){
        return null
      }else{
        return date
      }
    }
    def getText(x:Map[String, Any]): String={
      x.get("text").getOrElse("text", "").toString //Some aus JSON, daraus wieder einen String
    }
    def getLang(x:Map[String, Any]): String={
      x.get("lang").getOrElse("lang", "").toString //Some aus JSON, daraus wieder einen String
    }
    def checkisTweet(date: OffsetDateTime, userName: String): Boolean={
      var check = true
      if (date == null) check = false
      if (userName.equals("") || userName == null || userName == None) check = false
      return check
    }
    val tweet = JsonUtils.parseJson(jsonString)
    tweet match {
      case Some(x: Map[String, Any]) =>
        if (x.toString().contains("\"delete\":")){
          return None
        }
        val date = getdate(x)
        val text = getText(x)
        val lang = getLang(x)
        val userName = getUser(x)
        //println("User: " + date, text, lang, userName)
        if (checkisTweet(date, userName.toString)){
          val tweets = new Tweet(date: OffsetDateTime, userName: String, text: String, lang: String)
          Some(tweets.asInstanceOf[Tweet])
        }
        else None
      case None => None
    }
  }

  def getTwitterDate(date: String): OffsetDateTime = {
    try {
      OffsetDateTime.parse(date, dtf)
    } catch {
      case e: Exception =>
        //println(date)
        OffsetDateTime.now
    }
  }
}