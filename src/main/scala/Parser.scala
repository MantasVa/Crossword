import scala.annotation.tailrec

object Parser {

  def getWordsVertically(crossword: Option[Array[Array[Char]]]): Option[List[String]] = {
    getWordsHorizontally(crossword.map(_.transpose))
  }

  def getWordsHorizontally(crossword: Option[Array[Array[Char]]]): Option[List[String]] =
    crossword.flatMap(cr => {
      cr.foldLeft(None: Option[List[String]])((acc, row) => {
        @tailrec
        def go(row: Array[Char], startIndex: Int, patterns: Option[List[String]]): Option[List[String]] = {
          val spaceIndex = row.indexWhere(ch => ch == ' ', startIndex)
          val wordPattern = row.slice(startIndex, if (spaceIndex == -1) row.length else spaceIndex).mkString
          val nextStartIndex = if (spaceIndex != -1) row.indexWhere(ch => ch != ' ', spaceIndex) else -1

          nextStartIndex match {
            case _ if startIndex == row.length - 1 => patterns
            case -1 if wordPattern.length == 1 => patterns
            case -1 => Some(wordPattern :: patterns.getOrElse(Nil))
            case _ if wordPattern.length == 1 => go(row, nextStartIndex, patterns)
            case _ => go(row, nextStartIndex, Some(wordPattern :: patterns.getOrElse(Nil)))
          }
        }

        val startIndex = row.indexWhere(ch => ch != ' ')
        (acc, if (startIndex == -1) None else go(row, startIndex, None)) match {
          case (Some(a), Some(b)) => Some(a ::: b)
          case (ac, None) => ac
          case (None, xs) => xs
          case _ => None
        }

      })
    })
}