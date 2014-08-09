import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

class InputProcessor {

  var elementToRomanMapping = scala.collection.immutable.Map[String, String]()
  var mixedToCreditsMapping = scala.collection.immutable.Map[String, String]()
  var missingElementValues = scala.collection.immutable.Map[String, String]()
  var outputValueOfUnits = scala.collection.immutable.Map[String, String]()
  var outputValueOfCredits = scala.collection.immutable.Map[String, String]()
  var sequenceOfQuestions = scala.collection.immutable.List[Map[String, String]]()


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

    val splittedInput = line.split(" ")
    val endsRoman = createRegexForInputEndingWithRomanCharacter
    extractTheLastWordToMatchFrom(splittedInput) match {
      case endsRoman(roman) =>
        storeElementToRomanMapping(splittedInput)
      case "Credits" =>
        storeAppropriateMappings(line, splittedInput)
      case _ =>
        processTheQuestionPart(line, splittedInput)
    }
  }

  def extractTheLastWordToMatchFrom(splittedInput: Array[String]): String = {
    splittedInput(splittedInput.length - 1)
  }

  def storeElementToRomanMapping(words: Array[String]) {
    elementToRomanMapping = elementToRomanMapping + (words(0) -> words(2))
  }

  def checkForValidityOfQuestion(question: String): Boolean = {
    question.length > 30

  }

  def processTheQuestionPart(line: String, words: Array[String]) {
    val muchmany = words(1)
    muchmany match {
      case "much" =>
        val question = line.split(" is ")
        val x = Map(question(1) -> "")
        sequenceOfQuestions = sequenceOfQuestions.:+(x)
        outputValueOfUnits = outputValueOfUnits + (question(1) -> "")
      case "many" =>
        val question = line.split(" is ")
        val x = Map(question(1) -> "")
        sequenceOfQuestions = sequenceOfQuestions.:+(x)
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
