package gerferra
package optional
package samples

object MyAwesomeCommandLineTool extends optional.Application {

  // for instance...
  def main(count: Option[Int], file: Option[java.io.File], arg1: String) {

    val msg =
      s"""count: $count
         |file: ${file.map(_.getCanonicalPath)}
         |arg1: $arg1""".stripMargin

    println(msg)

  }

}