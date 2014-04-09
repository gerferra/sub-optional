package gerferra
package optional

import Logging._


trait Logging {

  trait Logger {

    def debug(str: String): Unit

  }

  protected lazy val log: Logger = {
    
    val loggerName = getClass.getSimpleName
    
    new Logger {
      def debug(str: String): Unit = {
        if (Level == Debug) {
          println(loggerName + ": " + str)
        }
      }
    }
  }

}

object Logging {

  sealed trait LogLevel
  object Debug extends LogLevel
  object None extends LogLevel

  val Level: LogLevel = None

}