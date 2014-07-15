package gerferra
package optional
package samples

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Future, Await, Promise }
import scala.concurrent.duration._
import scala.util.{ Try, Success, Failure }
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object usr extends optional.Application with Logging {

  def main(name: Option[String], limit: Option[Int], uid: Option[String], login: Option[String], debug: Option[Boolean]) {

    if (Seq(name, uid, login).flatten.isEmpty) {
      println("You must specify at least one search criteria")
      System.exit(1)
    }

    val effDebug = debug.getOrElse(false)

    val futUsrLDAPA = Future { ldapA.findUsr(name, limit, uid, login) }
    val futUsrLDAPB = Future { ldapB.findUsr(name, limit, uid, login) }
    val futUsrDB = Future { dbase.findUsr(name, limit, uid, login) }

    // V1
    /* can't use `onComplete` because there is no guarantee that it executes before program termination
    futUsrLDAPA.onComplete(printResult("--- LDAP A ---", debug = effDebug))
    futUsrLDAPB.onComplete(printResult("--- LDAP B ---", debug = effDebug))
    futUsrDB.onComplete(printResult("--- Data Base ---", debug = effDebug))
    
    val res = Await.ready(Future.sequence(Seq(futUsrLDAPA, futUsrLDAPB, futUsrDB)), Duration.Inf)
    // */

    // V2
    // /*
    val endLDAPA = futUsrLDAPA.continue(printResult("--- LDAP A ---", debug = effDebug))
    val endLDAPB = futUsrLDAPB.continue(printResult("--- LDAP B ---", debug = effDebug))
    val endDB = futUsrDB.continue(printResult("--- Data Base ---", debug = effDebug))

    val res = Await.ready(Future.sequence(Seq(endLDAPA, endLDAPB, endDB)), Duration.Inf)
    // */
  }

  implicit class FutureOps[T](val f: Future[T]) extends AnyVal {
    /**
     * Continues the computation of this future by taking the result
     *  of the current future and mapping it into another future.
     *
     *  The function `cont` is called only after the current future completes.
     *  The resulting future contains a value returned by `cont`.
     */
    def continue[S](cont: Try[T] => S): Future[S] = {
      val p = Promise[S]()
      f.onComplete { tt =>
        p.complete(Try(cont(tt)))
      }
      p.future
    }
  }

  type Error = String
  case class UsrInfo(name: String, id: String)

  def printResult(prefix: String, debug: Boolean)(res: Try[Either[Error, Iterable[UsrInfo]]]) {

    def printUsers(it: Iterable[UsrInfo]): Unit = {
      val str = it.map(showInfo).mkString("\n").toNel.getOrElse("<no results>")
      println(prefix)
      println(str)
      println()
    }

    def printError(err: Any): Unit = {
      println(prefix)
      println(err)
      if (debug && err.isInstanceOf[Exception]) err.asInstanceOf[Exception].printStackTrace
      println()
    }

    def printEither(either: Either[Error, Iterable[UsrInfo]]): Unit = {
      either.fold(printError, printUsers)
    }

    //Thread.sleep(1000)

    synchronized {
      res.fold(printEither, printError)
    }

  }

  def showInfo(info: UsrInfo): String = {
    import info._
    s"$name, id: $id"
  }

  val testData =
    Seq(
      UsrInfo("Some user", "suser1"),
      UsrInfo("Other", "other"),
      UsrInfo("Another", "another16"))

  trait Usr {

    def findUsrById(limit: Int)(id: String): Either[Error, Iterable[UsrInfo]]
    def findUsrByName(limit: Int)(name: String): Either[Error, Iterable[UsrInfo]]

    def findUsr(name: Option[String], limit: Option[Int], uid: Option[String], login: Option[String]): Either[Error, Iterable[UsrInfo]] = {

      val effLimit = limit.getOrElse(20)

      val optId = Seq(uid, login).flatten.map(_.toUpperCase).headOption

      val byId = optId.view.map { findUsrById(effLimit) }
      val byName = name.view.map { findUsrByName(effLimit) }

      val resTot = byId ++ byName

      val res = resTot.headOption

      res.getOrElse(Left("No search criteria specified"))

    }
  }

  object dbase extends Usr {

    def findUsrById(limit: Int)(id: String): Either[Error, Iterable[UsrInfo]] = {
      Right(Nil)
    }

    def findUsrByName(limit: Int)(name: String): Either[Error, Iterable[UsrInfo]] = {
      Right(testData.filter(_.name.toLowerCase.contains(name.toLowerCase)).take(limit))
    }

  }

  trait UsrLDAP extends Usr {

    def providerURL: String

    def findUsrById(limit: Int)(id: String): Either[Error, Iterable[UsrInfo]] = {
      Right(testData.filter(_.id.toLowerCase.contains(id.toLowerCase)).take(limit))
    }

    def findUsrByName(limit: Int)(name: String): Either[Error, Iterable[UsrInfo]] = {
      Right(Nil)
    }

  }

  object ldapA extends UsrLDAP { def providerURL: String = "ldap://ldapA/" }

  object ldapB extends UsrLDAP { def providerURL: String = "ldap://ldapB/" }

}
