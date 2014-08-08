import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

class InputProcessor {

  val elementToRomanMapping = new mutable.HashMap[String, String]()
  val mixedToCreditsMapping = new mutable.HashMap[String, String]()
  val missingElementValues = new mutable.HashMap[String, String]()
  val outputValueOfUnits = new mutable.HashMap[String, String]()
  val outputValueOfCredits = new mutable.HashMap[String, String]()

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
