import java.io.FileNotFoundException

import scala.collection.mutable.ListBuffer
import scala.io.Source

class InputProcessor {

  var elementToRomanMapping = scala.collection.immutable.Map[String, String]()
  var mixedToCreditsMapping = scala.collection.immutable.Map[String, String]()
  var missingElementValues = scala.collection.immutable.Map[String, String]()
  var outputValueOfUnits = scala.collection.immutable.Map[String, String]()
  var outputValueOfCredits = scala.collection.immutable.Map[String, String]()
  var outputValueOfConversion = scala.collection.immutable.Map[String, String]()
  var sequenceOfQuestions = scala.collection.immutable.List[Map[String, String]]()


  def readDataFromFileAndStoreMappings(fileName: String): Unit = {
    var input = new ListBuffer[String]()
    try {
      Source.fromFile(fileName).getLines().foreach(
        line => {
          input += line
        })
    }
    catch {
      case ex: FileNotFoundException => Utility.messageForFileNotFound(fileName)
    }
    processInput(input)
  }

  def processInput(input: ListBuffer[String]) = {
    input.foreach(line => {
      processLine(line)
    })
  }

  def processLine(line: String) = {
    val splittedInput = Utility.splitWith(line, " ")
    val endsRoman = RegularExpressions.createRegexForInputEndingWithRomanCharacter
    val endsWithCredits = RegularExpressions.createRegexForInputEndingWithCredits
    val question = RegularExpressions.createRegexForIdentifyingTheQuestion
    extractTheLastWordToMatchFrom(splittedInput) match {
      case endsRoman(roman) =>
        storeElementToRomanMapping(splittedInput)
      case endsWithCredits(capturedPart) =>
        storeAppropriateMappings(line, splittedInput)
      case question(capturedPart) =>
        processTheQuestionPart(line, splittedInput)
    }
  }


  def extractTheLastWordToMatchFrom(splittedInput: Array[String]): String = {
    splittedInput(splittedInput.length - 1)
  }

  def storeElementToRomanMapping(words: Array[String]) {
    elementToRomanMapping = elementToRomanMapping + (words(0) -> words(2))
  }

  def processTheQuestionPart(line: String, splittedInput: Array[String]) {
    val muchmany = Extractor.extractThePartUsedForDecidingTheTypeOfQuestionFrom(splittedInput)
    val regexForHowMuch = RegularExpressions.createRegexForQuestionOfTypeHowMuch()
    val regexForHowMany = RegularExpressions.createRegexForQuestionOfTypeHowMany()
    val regexForConversionBetweenMetals = RegularExpressions.createRegexForQuestionOfTypeWhichNeedConversionBetweenMetals()

    muchmany match {
      case regexForHowMuch(x) =>
        val question: Array[String] = extractAndStoreMappingForQuestion(line, " is ")
        outputValueOfUnits = outputValueOfUnits + (question(1) -> "")
      case regexForHowMany(x) =>
        val question: Array[String] = extractAndStoreMappingForQuestion(line, " is ")
        outputValueOfCredits = outputValueOfCredits + (question(1) -> "")
      case regexForConversionBetweenMetals(x) =>
        val question: Array[String] = extractAndStoreMappingForQuestion(line, "many ")
        outputValueOfConversion = outputValueOfConversion + (question(1) -> "")
      case _ =>
        sequenceOfQuestions = sequenceOfQuestions.:+(mappingForInvalidQuery)
    }
  }

  def mappingForInvalidQuery: Map[String, String] = Map("Invalid Query" -> "")

  def extractAndStoreMappingForQuestion(line: String, splitCriteria: String): Array[String] = {
    val question = Utility.splitWith(line, splitCriteria)
    val mapping = Map(question(1) -> "")
    storeMappingForSequenceOfQuestions(mapping)
    question
  }

  def storeMappingForSequenceOfQuestions(mapping: Map[String, String]) {
    sequenceOfQuestions = sequenceOfQuestions.:+(mapping)
  }

  def storeAppropriateMappings(mixedInput: String, words: Array[String]) {
    mixedToCreditsMapping = mixedToCreditsMapping + (mixedInput -> words(words.length - 2))
    missingElementValues = missingElementValues + (words(2) -> "")
  }
}
