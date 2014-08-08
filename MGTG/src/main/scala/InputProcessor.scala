import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

class InputProcessor {

  var elementToRomanMapping = new mutable.HashMap[String, String]()
  var mixedToCreditsMapping = new mutable.HashMap[String, String]()
  var missingElementValues = new mutable.HashMap[String, String]()
  var outputValueOfUnits = new mutable.HashMap[String, String]()
  var outputValueOfCredits = new mutable.HashMap[String, String]()
  var invalidQueries = new ListBuffer[String]

  def readDataFromFileAndStoreMappings(fileName: String): Unit = {
    var input = new ListBuffer[String]()
    val d = Source.fromFile(fileName).getLines().foreach(
      line => {
        input += line
      })
    processInput(input)
  }

  def processInput(input: ListBuffer[String]) = {
    input.foreach(line => {
      processLine(line)
    })
  }

  def processLine(line: String) = {
    val words = line.split(" ")
    val endsRoman = createRegexForInputEndingWithRomanCharacter
    words(words.length - 1) match {
      case endsRoman(roman) =>
        elementToRomanMapping.put(words(0), words(2))
      case "Credits" =>
        mixedToCreditsMapping.put(line, words(words.length - 2))
        missingElementValues.put(words(2), "")
      case _ =>
        val muchmany = words(1)
        muchmany match {
          case "much" =>
            val question = line.split(" is ")
            outputValueOfUnits.put(question(1), "")
          case "many" =>
            val question = line.split(" is ")
            outputValueOfCredits.put(question(1), "")
        }
    }
  }

  def createRegexForInputEndingWithRomanCharacter: Regex = {
    "([IVLX])".r
  }
}
