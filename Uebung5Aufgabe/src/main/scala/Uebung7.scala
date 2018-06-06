object Uebung7 {

  def aufgabe1a(Paradigmen: List[(String, String)]) {
    def forloop(l: List[(String, String)], bed: String): List[String] = {
      for (lang <- l if lang._2 == bed) yield lang._1
    }

    def filtermapflatmap(l: List[(String, String)], bed: String): List[String] = {
      l.filter(ele => ele._2 == bed).map(langs => langs._1)
    }
    println(forloop(Paradigmen,"objektorientiert"))
    println(filtermapflatmap(Paradigmen,"objektorientiert"))
  }

  def aufgabe1b(Paradigmen: List[(String, String)]): Unit = {
    def forloop(l: List[(String, String)], bed: List[String]): List[List[String]] = {
      for (lang <- bed) yield
        for (la <- l if la._1 == lang) yield la._2
    }

    def filtermapflatmap(l: List[(String, String)], bed: List[String]): List[List[String]] = {
      bed.map(lang => l.filter(ele => ele._1 == lang).map(langs => langs._2))
    }
    println(forloop(Paradigmen, List("scala","erlang")))
    println(filtermapflatmap(Paradigmen,List("scala","erlang")))
  }

  def aufgabe1c(Paradigmen: List[(String, String)]): Unit = {
    /*def forloop(l: List[(String, String)], bed: List[String]): List[List[String]] = {
      for (lang <- bed) yield
        for (la <- l if la._1 == lang) yield la._2
    }

    def filtermapflatmap(l: List[(String, String)], bed: List[String]): List[List[String]] = {
      bed.map(lang => l.filter(ele => ele._1 == lang).map(langs => langs._2))
    }
    println(forloop(Paradigmen, List("scala","erlang")))
    println(filtermapflatmap(Paradigmen,List("scala","erlang")))*/
    println("ToDo Aufgabe 1. c)")
  }

  def aufgabe1d(Paradigmen: List[(String, String)]): Unit = {
    /*def forloop(l: List[(String, String)], bed: List[String]): List[List[String]] = {
      for (lang <- bed) yield
        for (la <- l if la._1 == lang) yield la._2
    }

    def filtermapflatmap(l: List[(String, String)], bed: List[String]): List[List[String]] = {
      bed.map(lang => l.filter(ele => ele._1 == lang).map(langs => langs._2))
    }
    println(forloop(Paradigmen, List("scala","erlang")))
    println(filtermapflatmap(Paradigmen,List("scala","erlang")))*/
    println("ToDo Aufgabe 1. d)")
  }

  def aufgabe2(): Unit={
    println("ToDo Aufgabe 3")
  }

  def aufgabe3(): Unit={
    println("ToDo Aufgabe 4")
  }

  def main(args: Array[String]): Unit = {
    var Paradigmen=List(("erlang", "funktional"), ("erlang", "logisch"), ("prolog", "logisch"),
      ("scala", "funktional"), ("scala", "objektorientiert"), ("scala", "logisch"),
      ("java","objektorientiert"))

    //aufrufe Aufgabe 1 a)
    aufgabe1a(Paradigmen)

    //Aufgabe 1 b)
    aufgabe1b(Paradigmen)

    //Aufgabe 1c)
    aufgabe1c(Paradigmen)

    //Aufgabe 1d)
    aufgabe1d(Paradigmen)

    //Aufgabe 2
    aufgabe2()

    //Aufgabe 3
    aufgabe3()
  }
}
