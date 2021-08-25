import scala.io.Source

object IO {

  def getHeadLine(file: java.io.File): Option[String] = {
    val src = Source.fromFile(file)
    try {
      Some(src.bufferedReader.readLine)
    } finally {
      src.close()
    }
  }

  def getTail(file: java.io.File): Option[List[String]] = {
    val src = Source.fromFile(file)
    try {
      Some(src.getLines.drop(1).toList)
    } finally {
      src.close()
    }
  }

}