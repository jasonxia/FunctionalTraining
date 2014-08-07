package com.rea.typesafety


object OptionalExercises1 {
  val config = Map[String, String]("host" -> "rea.com", "port" -> "8080")

  def getFromConfig(key: String): Option[String] = config.get(key)

  def lengthOfHost(): Option[Int] = getFromConfig("host") map (_.length)

  def portPlus1000(): Option[Int] = getFromConfig("port") map (_.toInt + 1000)
}

object OptionalExercises2 {

  val hosts = Map("host1" -> "rea.com", "host2" -> "test.rea.com", "host3" -> "netflix.com")
  val envs = Map("rea.com" -> "prod", "test.rea.com" -> "test", "amazon.com" -> "stage")

  def getEnvForHost(host: String): String = envs.get(hosts.getOrElse(host,"")) match {
    case Some(env) => env
    case None => "couldn't resolve"
  }

  // See how many ways you can implement this.
  // Will either return "Connected to rea.com" or "not connected"
  def connectToReaHostsOnly(host: String): String = hosts.get(host) match {
    case Some(h) if h contains ("rea.com") => createConnection(h)
    case _ => "not connected"
  }

  def createConnection(domain: String): String = s"connected to $domain"

}

object OptionalExercises3 {

  sealed trait Maybe[+A]

  case class Just[A](get: A) extends Maybe[A]

  case object Nothing extends Maybe[Nothing]

  def flatMap[A, B](m: Maybe[A], f: A => Maybe[B]): Maybe[B] = m match {
    case Just(v) => f(v)
    case Nothing => Nothing
  }

  def map[A, B](m: Maybe[A], f: A => B): Maybe[B] = m match {
    case Just(v) => Just(f(v))
    case Nothing => Nothing
  }

  def fold[A, B](m: Maybe[A], default: => B, f: A => B): B = m match {
    case Just(v) => f(v)
    case Nothing => default
  }

  def orElse[A](m: Maybe[A], otherwise: => Maybe[A]): Maybe[A] = m match {
    case Nothing => otherwise
    case Just(v) => m
  }

  def orSome[A](m: Maybe[A], default: => A): A = m match {
    case Nothing => default
    case Just(v) => v
  }

  def map2[A, B, C](f: (A, B) => C)(m1: Maybe[A], m2: Maybe[B]): Maybe[C] = (m1, m2) match {
    case (Just(a), Just(b)) => Just(f(a, b))
    case _ => Nothing
  }

  def sequence[A](list: List[Maybe[A]]): Maybe[List[A]] =
    list.foldLeft[Maybe[List[A]]](Just(Nil)) {
      (acc: Maybe[List[A]], a: Maybe[A]) => (acc, a) match {
        case (Just(l), Just(v)) => Just(l ++ List(v))
        case _ => Nothing
      }
    }

  def collect[A](list: List[Maybe[A]]): Maybe[List[A]] =
    sequence(list.filter(_ != Nothing))
}

