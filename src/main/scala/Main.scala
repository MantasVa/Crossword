import java.io.File

object Main extends App {
  val filename = "crossword.txt"
  val file = new File(s"${System.getProperty("user.dir")}\\$filename")

  val words = IO.getHeadLine(file).map(_.split(" ")).map(_.toList)
  val crossword = IO.getTail(file).map(_.toArray.map(s => s.toCharArray))

  val horizontalPatterns = Parser.getWordsHorizontally(crossword)
  val verticalPatterns = Parser.getWordsVertically(crossword)

  val patterns = (horizontalPatterns, verticalPatterns) match {
    case (Some(as), Some(xs)) => Some(as ::: xs)
    case (_, s) => s
    case (s, _) => s
    case _ => None
  }
  val wordPatternsOption = CrosswordSolver.fitWordsIntoPatterns(patterns, words)
  val solvedCrosswordOption = CrosswordSolver.solveCrosswordHorizontally(crossword, 0, crossword.get(0).indexWhere(_ != ' '), wordPatternsOption)

  solvedCrosswordOption.getOrElse(Array()) foreach { row => row foreach print; println }
}







