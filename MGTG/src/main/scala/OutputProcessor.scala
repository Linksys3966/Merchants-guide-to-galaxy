import scala.util.matching.Regex

class OutputProcessor(inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal) {

  def readFileAndStoreMappings() {
    inputProcessor.readDataFromFileAndStoreMappings("/Users/vivekpatil/Data.txt")
  }

  def calculateMissingValuesForAllMappings() = {
    val mappings = inputProcessor.mixedToCreditsMapping
    mappings.map(mappingTuple => {
      val (splittedInput: Array[String], missingElementValue: Double) = calculateMissingValueForIndividualMapping(mappingTuple)
      storeMissingElementValueInMap(splittedInput, missingElementValue)
    })
  }

  def calculateMissingValueForIndividualMapping(mappingTuple: (String, String)): (Array[String], Double) = {
    val mixedInput = mappingTuple._1
    val credits = mappingTuple._2.toDouble
    val splittedInput = splitWith(mixedInput, " ")
    val intergalsaticInputs = getIntergalasticInputFrom(splittedInput)
    val collectionOfIntergalasticUnits = splitWith(intergalsaticInputs, "#")
    val romanInput: String = getRomanInputFromCollectionOfIntergalasticUnits(collectionOfIntergalasticUnits)
    val decimalValue = romanToDecimal.convertRomanToDecimal(romanInput)
    val missingElementValue: Double = logicForComputingMissingValue(credits, decimalValue)
    (splittedInput, missingElementValue)
  }

  def storeMissingElementValueInMap(splittedInput: Array[String], missingElementValue: Double): Unit = {
    inputProcessor.missingElementValues = inputProcessor.missingElementValues + (extractElementFrom(splittedInput) -> missingElementValue.toString)
  }

  def extractElementFrom(splittedInput: Array[String]): String = {
    splittedInput(2)
  }

  def logicForComputingMissingValue(credits: Double, decimalValue: Int): Double = {
    val missingElementValue: Double = credits / decimalValue
    missingElementValue
  }

  def getRomanInputFromCollectionOfIntergalasticUnits(collectionOfIntergalasticUnits: Array[String]): String = {
    val romanInput = new StringBuilder
    collectionOfIntergalasticUnits.map(
      interGalasticUnit => {
        romanInput.append(getRomanEquivalentForGivenIntergalasticInput(interGalasticUnit))
      })
    romanInput.toString()
  }

  def getRomanEquivalentForGivenIntergalasticInput(interGalasticUnit: String): String = {
    inputProcessor.elementToRomanMapping(interGalasticUnit)
  }

  def splitWith(mixedInput: String, splitCriteria: String): Array[String] = {
    mixedInput.split(splitCriteria)
  }

  def getIntergalasticInputFrom(mixedInput: Array[String]): String = {
    val intergalasticInput = new StringBuilder
    for (i <- 0 until mixedInput.length) {
      if (i < 2)
        intergalasticInput.append(mixedInput(i) + "#")

    }
    intergalasticInput.toString()
  }

  def calculateLengthOfArray(s: String): Int = {
    val splittedInput = splitWith(s, " ")
    splittedInput.length
  }

  def readSequenceOfQuestionsAndCalculateAnswer() = inputProcessor.sequenceOfQuestions.map(question => {
    val x = question
    question.map(s => {
      val length: Int = calculateLengthOfArray(s._1)
      length match {
        case 3 =>
          val (question: Array[String], answer: Double) = calculateHowManyCreditsForIndividualMapping(s)
          printFormattedOutput(question, answer)
        case 4 =>
          val (question: Array[String], answer: Double) = calculateHowMuchIsTheValueForIndividualMapping(s)
          printFormattedOutput(question, answer)
      }
    })
  })

  def calculateHowMuchIsTheValueForIndividualMapping(mapping: (String, String)): (Array[String], Double) = {
    val question = extractQuestion(mapping._1).split(" ")
    val romanInput: String = getRomanInputFromCollectionOfIntergalasticUnits(question)
    val answer = romanToDecimal.convertRomanToDecimal(romanInput.toString())
    (question, answer)
  }

  def printFormattedOutput(question: Array[String], answer: Double) = {
    question.map(ques => {
      print(ques + " ")
    })
    print("is " + answer)
    println()
  }

  def extractQuestion(tuple: String): String = tuple.substring(0, tuple.length - 1)

  def calculatehowManyCreditsForAllMappings() = {
    val mappings = inputProcessor.outputValueOfCredits

    mappings.map(mapping => {
      val (question: Array[String], answer: Double) = calculateHowManyCreditsForIndividualMapping(mapping)
      printFormattedOutput(question, answer)
    })
  }


  def calculateHowManyCreditsForIndividualMapping(mapping: (String, String)): (Array[String], Double) = {
    val question = extractQuestion(mapping._1).split(" ")
    val romanInput = new StringBuilder
    var elementValue: Double = 0
    val unit = getRegexForIntergalasticUnit
    val element = getRegexForElementOnEarth
    def mapQuestiontoGetRomanAndElementValue {
      question.map {
        case unit(unit) =>
          romanInput.append(getRomanEquivalentForGivenIntergalasticInput(unit))
        case element(element) =>
          elementValue = inputProcessor.missingElementValues(element).toDouble
        case _ =>
      }
    }
    mapQuestiontoGetRomanAndElementValue
    val decimalValue = romanToDecimal.convertRomanToDecimal(romanInput.toString())
    val answer = decimalValue * elementValue
    (question, answer)
  }

  def getRegexForElementOnEarth: Regex = {
    "([A-Z][a-z]+)".r
  }

  def getRegexForIntergalasticUnit: Regex = {
    "([a-z]+{0,4})".r
  }

}
