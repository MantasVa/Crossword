import models.CrossingWordPattern

object CrosswordSolver {
  private val vowels = Array('A', 'E', 'I', 'O', 'U', 'Y')
  private val consonants = Array('B', 'C', 'D', 'F', 'G', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'S', 'T', 'V', 'X', 'Z', 'H', 'R', 'W', 'Y')
  private val vowelMarking = 'O'
  private val consonantMarking = 'X'

  def fitWordsIntoPatterns(patternsOption: Option[List[String]], wordsOption: Option[List[String]]): Option[List[(String, String)]] = {
    val groupedPatterns = patternsOption.map(_.groupBy(_.length))
    val groupedWords = wordsOption.map(_.groupBy(_.length))

    groupedPatterns.flatMap(_.foldLeft(None: Option[List[(String, String)]])((answers, groupedPatterns) => {
      var locAnswers = answers
      val words = groupedWords.flatMap(_.get(groupedPatterns._1))
      groupedPatterns._2.foldLeft(words)((pw, s) => {
        s.zipWithIndex.foldLeft(pw)((words, t) => {
          words.map(_.filter(w => {
            if (t._1 == vowelMarking) vowels.contains(w(t._2))
            else consonants.contains(w(t._2))
          }))
        }).flatMap(mw => {
          locAnswers = if (locAnswers.isEmpty) Some(List((mw.head, s))) else locAnswers.map(l => (mw.head, s) :: l)
          pw.map(_.filter(s => s != mw.head))
        })
      })
      locAnswers
    }))
  }

  def solveCrosswordHorizontally(crossword: Option[Array[Array[Char]]],
                                 rowIndex: Int,
                                 startIndex: Int,
                                 wordsOption: Option[List[(String, String)]]): Option[Array[Array[Char]]] = {
    crossword.flatMap(cr => {
      val row = cr(rowIndex)
      val spaceIndex = row.indexWhere(c => c == ' ', startIndex)
      val wordPattern = row.slice(startIndex, if (spaceIndex == -1) row.length else spaceIndex)

      def goToNextStep(arr: Array[Array[Char]], leftWords: Option[List[(String, String)]]): Option[Array[Array[Char]]] = {
        val nextStartIndex = if (spaceIndex != -1) row.indexWhere(_ != ' ', spaceIndex) else -1
        nextStartIndex match {
          case -1 if rowIndex + 1 < arr.length => solveCrosswordHorizontally(Some(arr), rowIndex + 1, cr(rowIndex + 1).indexWhere(c => c != ' '), leftWords)
          case -1 => solveCrosswordVertically(Some(arr), 0, cr.transpose.head.indexWhere(c => c != ' '), leftWords)
          case n => solveCrosswordHorizontally(Some(arr), rowIndex, n, leftWords)
        }
      }

      if (startIndex == row.length - 1 || wordPattern.length == 1) {
        goToNextStep(cr, wordsOption)
      } else {
        val endIndex = if (spaceIndex == -1) row.length else spaceIndex

        val crossingPatterns = wordPattern.zip(startIndex until endIndex).foldLeft(None: Option[List[CrossingWordPattern]])((l, wpi) => {
          if ((rowIndex > 0 && isPatternMarking(cr(rowIndex - 1)(wpi._2)))
            || rowIndex < cr.length - 1 && isPatternMarking(cr(rowIndex + 1)(wpi._2))) {

            val crossingColumn = getColumn(wpi._2, cr)
            val patternEndIndex = if (crossingColumn.indexOf(' ', rowIndex) == -1) row.length else crossingColumn.indexOf(' ', rowIndex)
            val upperSideColumn = crossingColumn.dropRight(crossingColumn.length - patternEndIndex)
            val patternStartIndex = if (upperSideColumn.lastIndexOf(' ') == -1) 0 else upperSideColumn.lastIndexOf(' ') + 1
            val crossingPattern = if (patternStartIndex > 0) upperSideColumn.drop(patternStartIndex) else upperSideColumn

            Some(l.getOrElse(Nil) :+ CrossingWordPattern(crossingPattern, rowIndex - patternStartIndex, wpi._2 - startIndex))
          }
          else {
            l
          }
        })
        val suitableWords = getSuitableWords(wordPattern, wordsOption).flatMap(sw => {
          val crossingSuitableWords = crossingPatterns.map(xs => xs.map(cp => (cp, getSuitableWords(cp.crossingPattern, wordsOption).getOrElse(Nil))))
          crossingSuitableWords.map(cw => {
            cw.foldLeft(sw)((l, cp) => {
              l.filter(w => cp._2.exists(cw => cw._1(cp._1.crossingPatternIndex) == w._1(cp._1.crossedAtIndex)))
            })
          })
        })

        val suitableWordsCount = suitableWords.getOrElse(Nil).length
        if (suitableWordsCount > 1) {
          suitableWords.flatMap(_.foldLeft(None: Option[Array[Array[Char]]])((l, w) => {
            val leftWords = wordsOption.map(xs => xs.filterNot(_._1.sameElements(w._1)))
            val updatedCrossword = updateCrossword(w._1, cr, rowIndex, (startIndex, spaceIndex))

            val finalCrossword = goToNextStep(updatedCrossword, leftWords)
            if (finalCrossword.isEmpty) l else finalCrossword
          }))
        } else if (suitableWordsCount == 1) {
          val selectedWord = suitableWords.get.head
          val leftWords = wordsOption.map(xs => xs.filterNot(_._1.sameElements(selectedWord._1)))
          val updatedCrossword = updateCrossword(selectedWord._1, cr, rowIndex, (startIndex, spaceIndex))

          goToNextStep(updatedCrossword, leftWords)
        }
        else {
          None
        }
      }
    }
    )
  }

  def solveCrosswordVertically(crossword: Option[Array[Array[Char]]],
                               rowIndex: Int,
                               startIndex: Int,
                               wordsOption: Option[List[(String, String)]]): Option[Array[Array[Char]]] = {
    crossword.flatMap(cr => {
      val crt = cr.transpose
      val row = crt(rowIndex)
      val spaceIndex = row.indexWhere(c => c == ' ', startIndex)
      val wordPattern = row.slice(startIndex, if (spaceIndex == -1) row.length else spaceIndex)

      def goToNextStep(arr: Array[Array[Char]], leftWords: Option[List[(String, String)]]): Option[Array[Array[Char]]] = {
        val nextStartIndex = if (spaceIndex != -1) row.indexWhere(_ != ' ', spaceIndex) else -1
        nextStartIndex match {
          case -1 if rowIndex + 1 < arr.length => solveCrosswordVertically(Some(arr), rowIndex + 1, crt(rowIndex + 1).indexWhere(c => c != ' '), leftWords)
          case -1 => Some(arr)
          case n => solveCrosswordVertically(Some(arr), rowIndex, n, leftWords)
        }
      }

      if (startIndex == row.length - 1 || wordPattern.length == 1) {
        goToNextStep(cr, wordsOption)
      } else {
        val suitableWords = getSuitableWords(wordPattern, wordsOption)
        val suitableWordsCount = suitableWords.getOrElse(Nil).length

        if (suitableWordsCount > 1) {
          suitableWords.flatMap(_.foldLeft(None: Option[Array[Array[Char]]])((l, w) => {
            val leftWords = wordsOption.map(xs => xs.filterNot(_._1.sameElements(w._1)))
            val updatedCrossword = updateCrossword(w._1, crt, rowIndex, (startIndex, spaceIndex))

            val finalCrossword = goToNextStep(updatedCrossword.transpose, leftWords)
            if (finalCrossword.isEmpty) l else finalCrossword
          }))
        } else if (suitableWordsCount == 1) {
          val selectedWord = suitableWords.get.head
          val leftWords = wordsOption.map(xs => xs.filterNot(_._1.sameElements(selectedWord._1)))
          val updatedCrossword = updateCrossword(selectedWord._1, crt, rowIndex, (startIndex, spaceIndex))
          goToNextStep(updatedCrossword.transpose, leftWords)
        }
        else {
          None
        }

      }
    })
  }

  def getSuitableWords(pattern: Array[Char], wordsOption: Option[List[(String, String)]]): Option[List[(String, String)]] = {
    if (pattern.zipWithIndex.exists(t => !isPatternMarking(t._1))) {
      wordsOption.map(_.filter(t => t._2.length == pattern.length &&
        t._1.zipWithIndex.foldLeft(true)((b, li) => {
          if (!b) {
            false
          }
          else if (isPatternMarking(pattern(li._2))) {
            t._2(li._2) == pattern(li._2)
          } else {
            li._1 == pattern(li._2)
          }
        })))
    } else {
      wordsOption.map(_.filter(t => t._2.sameElements(pattern)))
    }

  }

  def updateCrossword(word: String, crossword: Array[Array[Char]], rowIndex: Int, startSpaceIndexes: (Int, Int)): Array[Array[Char]] = {
    Array.tabulate(crossword.length, crossword.head.length) { (i, y) =>
      if (i == rowIndex && y >= startSpaceIndexes._1 && (y < startSpaceIndexes._2 || startSpaceIndexes._2 == -1)) {
        word(y - startSpaceIndexes._1)
      } else {
        crossword(i)(y)
      }
    }
  }

  def getColumn(i: Int, arr: Array[Array[Char]]): Array[Char] = arr.map(_ (i))

  def isPatternMarking(ch: Char): Boolean =
    ch == consonantMarking || ch == vowelMarking
}
