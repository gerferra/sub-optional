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

  def main(nombre: Option[String], cantidad: Option[Int], uid: Option[String], cedula: Option[Int], login: Option[String], debug: Option[Boolean]) {

    val effDebug = debug.getOrElse(false)

    if (Seq(nombre, uid, cedula, login).flatten.isEmpty) {
      println("Debe especificar algún criterio de búsqueda")
      System.exit(1)
    }

    val futUsrLDAPA = Future { ldapA.findUsr(nombre, cantidad, uid, cedula, login) }
    val futUsrLDAPB = Future { ldapB.findUsr(nombre, cantidad, uid, cedula, login) }
    val futUsrDB = Future { dbase.findUsr(nombre, cantidad, uid, cedula, login) }

    /*
     * no puedo usar `onComplete` porque no puedo esperar a su finalización y 
     * por lo tanto no hay garantías que se imprima el resultado
    futUsrLDAPA.onComplete(printResult("--- LDAP A ---", debug = effDebug))
    futUsrLDAPB.onComplete(printResult("--- LDAP B ---", debug = effDebug))
    futUsrBD.onComplete(printResult("--- Base Datos ---", debug = effDebug))

    val res = Await.ready(Future.sequence(Seq(futUsrLDAPA, futUsrLDAPB, futUsrBD)), Duration.Inf)
	*/
    
    val endLDAPA = futUsrLDAPA.continue(printResult("--- LDAP A ---", debug = effDebug))
    val endLDAPB = futUsrLDAPB.continue(printResult("--- LDAP B ---", debug = effDebug))
    val endDB = futUsrDB.continue(printResult("--- Data Base ---", debug = effDebug))
    
    val res = Await.ready(Future.sequence(Seq(endLDAPA, endLDAPB, endDB)), Duration.Inf)

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
  type UsrInfo = String

  def printResult(prefix: String, debug: Boolean)(res: Try[Either[Error, Iterable[UsrInfo]]]) {

    def printUsers(it: Iterable[UsrInfo]): Unit = {
      val str = it.map(showInfo).mkString("\n\n").toNel.getOrElse("<sin resultados>")
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

    synchronized {
      res.fold(printEither, printError)
    }

  }

  def showInfo(info: UsrInfo): String = {
    info
  }

  val testData =
    Seq(
      "Some user 1",
      "Other 999",
      "Another 60")


  trait Usr {

    def findUsrById(limit: Int)(id: String): Either[Error, Iterable[UsrInfo]]
    def findUsrByCedula(limit: Int)(cedula: Int): Either[Error, Iterable[UsrInfo]]
    def findUsrByName(limit: Int)(name: String): Either[Error, Iterable[UsrInfo]]

    def findUsr(nombre: Option[String], cantidad: Option[Int], uid: Option[String], cedula: Option[Int], login: Option[String]): Either[Error, Iterable[UsrInfo]] = {

      val limit = cantidad.getOrElse(20)

      val optId = Seq(uid, login).flatten.map(_.toUpperCase).headOption

      val byId = optId.view.map { findUsrById(limit) }
      val byCed = cedula.view.map { findUsrByCedula(limit) }
      val byName = nombre.view.map { findUsrByName(limit) }

      val resTot = byId ++ byCed ++ byName

      val res = resTot.headOption

      res.getOrElse(Left("No se especificó ningún criterio de búsqueda"))

    }
  }

  trait UsrLDAP extends Usr {

    def providerURL: String

    def findUsrById(limit: Int)(id: String): Either[Error, Iterable[UsrInfo]] = {
      Right(testData)
    }

    def findUsrByCedula(limit: Int)(cedula: Int): Either[Error, Iterable[UsrInfo]] = {
      Left("<Búsqueda por cédula no soportada en LDAP>")
    }

    def findUsrByName(limit: Int)(name: String): Either[Error, Iterable[UsrInfo]] = {
      Right(testData)
    }

  }

  object ldapA extends UsrLDAP { def providerURL: String = "ldap://ldapA/" }

  object ldapB extends UsrLDAP { def providerURL: String = "ldap://ldapB/" }

  object dbase extends Usr {

    def findUsrById(limit: Int)(id: String): Either[Error, Iterable[UsrInfo]] = {
      Right(testData)
    }

    def findUsrByCedula(limit: Int)(cedula: Int): Either[Error, Iterable[UsrInfo]] = {
      Right(testData)
    }

    def findUsrByName(limit: Int)(name: String): Either[Error, Iterable[UsrInfo]] = {
      Right(testData)
    }

  }

}
