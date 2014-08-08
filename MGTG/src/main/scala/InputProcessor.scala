import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

class InputProcessor {

  var elementToRomanMapping = scala.collection.immutable.Map[String, String]()
  var mixedToCreditsMapping = scala.collection.immutable.Map[String, String]()
  var missingElementValues = scala.collection.immutable.Map[String, String]()
  var outputValueOfUnits = scala.collection.immutable.Map[String, String]()
  var outputValueOfCredits = scala.collection.immutable.Map[String, String]()

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
        elementToRomanMapping = elementToRomanMapping + (words(0) -> words(2))
      case "Credits" =>
        storeAppropriateMappings(line, words)
      case _ =>
        processTheQuestionPart(line, words)
    }
  }

  def processTheQuestionPart(line: String, words: Array[String]) {
    val muchmany = words(1)
    muchmany match {
      case "much" =>
        val question = line.split(" is ")
        outputValueOfUnits = outputValueOfUnits + (question(1) -> "")
      case "many" =>
        val question = line.split(" is ")
        outputValueOfCredits = outputValueOfCredits + (question(1) -> "")
    }
  }

  def storeAppropriateMappings(line: String, words: Array[String]) {
    mixedToCreditsMapping = mixedToCreditsMapping + (line -> words(words.length - 2))
    missingElementValues = missingElementValues + (words(2) -> "")
  }

  def createRegexForInputEndingWithRomanCharacter: Regex = {
    "([IVLX])".r
  }
}
