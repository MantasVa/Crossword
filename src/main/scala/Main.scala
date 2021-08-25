import java.io.File

object Main extends App {
  val filename = "crossword.txt"
  val file = new File(s"${System.getProperty("user.dir")}\\$filename")

  val words = IO.getHeadLine(file).map(_.split(" ")).map(_.toList)
  val tail = IO.getTail(file)

  val crosswordUnsolved = tail.map(_.toArray.map(s => s.toCharArray))

  val horizontalWordPatterns = Parser.getWordsHorizontally(crosswordUnsolved)
  val verticalWordPatterns = Parser.getWordsVertically(crosswordUnsolved)

  val patterns = (horizontalWordPatterns, verticalWordPatterns) match {
    case (Some(as), Some(xs)) => Some(as ::: xs)
    case (_, s) => s
    case (s, _) => s
    case _ => None
  }
  val solved = CrosswordSolver.fitWordsIntoPatterns(patterns, words)
  println(solved)

  val option = CrosswordSolver.solveCrosswordHorizontally(crosswordUnsolved, 0, crosswordUnsolved.get(0).indexWhere(_ != ' '), solved)
  option.get foreach { row => row foreach print; println }
}







