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

  def mappingForInvalidQuery: Map[String,String] = Map("Invalid Query"->"")

  def processTheQuestionPart(line: String, splittedInput: Array[String]) {
    val muchmany = extractThePartUsedForDecidingTheTypeOfQuestion(splittedInput)
    val regexForHowMuch = createRegexForQuestionOfTypeHowMuch()
    val regexForHowMany = createRegexForQuestionOfTypeHowMany()
    muchmany match {
      case regexForHowMuch(x) =>
        val question: Array[String] = extractAndStoreMappingForQuestion(line)
        outputValueOfUnits = outputValueOfUnits + (question(1) -> "")
      case regexForHowMany(x) =>
        val question: Array[String] = extractAndStoreMappingForQuestion(line)
        outputValueOfCredits = outputValueOfCredits + (question(1) -> "")
      case _=>
        sequenceOfQuestions = sequenceOfQuestions.:+(mappingForInvalidQuery)
    }
  }

  def extractThePartUsedForDecidingTheTypeOfQuestion(splittedInput: Array[String]): String = {
    splittedInput(1) + " " + splittedInput(2)
  }

  def createRegexForQuestionOfTypeHowMuch(): Regex = "(much is)".r

  def createRegexForQuestionOfTypeHowMany(): Regex = "(many Credits)".r

  def extractAndStoreMappingForQuestion(line: String): Array[String] = {
    val question = splitWith(line, " is ")
    val mapping = Map(question(1) -> "")
    storeMappingForSequenceOfQuestions(mapping)
    question
  }

  def splitWith(line: String, criteria: String): Array[String] = line.split(criteria)

  def storeMappingForSequenceOfQuestions(x: Map[String, String]) {
    sequenceOfQuestions = sequenceOfQuestions.:+(x)
  }

  def storeAppropriateMappings(mixedInput: String, words: Array[String]) {
    mixedToCreditsMapping = mixedToCreditsMapping + (mixedInput -> words(words.length - 2))
    missingElementValues = missingElementValues + (words(2) -> "")
  }

  def createRegexForInputEndingWithRomanCharacter: Regex = {
    "([IVLX])".r
  }

  def checkForValidityOfQuestion(question: String): Boolean = {
    question.length > 30

  }
}
