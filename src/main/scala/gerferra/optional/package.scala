package gerferra

import scala.util.{ Try, Success, Failure }

package object optional {

  implicit class TryHarder[T](val t: Try[T]) extends AnyVal {
    def toEither: Either[Throwable, T] = t match {
      case Success(s) => Right(s)
      case Failure(f) => Left(f)
    }
    def fold[U](success: T => U, failure: Throwable => U): U = toEither.fold(failure, success)
  }

  
  implicit class SeqOps[T](val s: Seq[T]) extends AnyVal {
    
    def toNel: Option[Seq[T]] = if(s.isEmpty) None else Some(s)
    
  }
  
  implicit class StringOps(val s: String) extends AnyVal {
    
    def toNel: Option[String] = if(s.isEmpty) None else Some(s)
    
  }
}