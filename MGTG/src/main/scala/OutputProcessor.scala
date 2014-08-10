

class OutputProcessor() {

  def readFileAndStoreMappings(inputProcessor: InputProcessor) {
    inputProcessor.readDataFromFileAndStoreMappings("/Users/vivekpatil/Data.txt")
  }

  def calculateMissingValuesForAllMappings(inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal) = {
    val mappings = inputProcessor.mixedToCreditsMapping
    mappings.map(mappingTuple => {
      val (splittedInput: Array[String], missingElementValue: Double) = calculateMissingValueForIndividualMapping(mappingTuple, romanToDecimal, inputProcessor)
      storeMissingElementValueInMap(splittedInput, missingElementValue, inputProcessor)
    })
  }

  def calculateMissingValueForIndividualMapping(mappingTuple: (String, String), romanToDecimal: RomanToDecimal, inputProcessor: InputProcessor): (Array[String], Double) = {
    val mixedInput = mappingTuple._1
    val credits = mappingTuple._2.toDouble
    val splittedInput = splitWith(mixedInput, " ")
    val intergalsaticInputs = getIntergalasticInputFrom(splittedInput)
    val collectionOfIntergalasticUnits = splitWith(intergalsaticInputs, "#")
    val romanInput: String = getRomanInputFromCollectionOfIntergalasticUnits(collectionOfIntergalasticUnits, inputProcessor)
    val decimalValue = romanToDecimal.convertRomanToDecimal(romanInput)
    val missingElementValue: Double = logicForComputingMissingValue(credits, decimalValue)
    (splittedInput, missingElementValue)
  }

  def logicForComputingMissingValue(credits: Double, decimalValue: Int): Double = {
    val missingElementValue: Double = credits / decimalValue
    missingElementValue
  }

  def getIntergalasticInputFrom(mixedInput: Array[String]): String = {
    val intergalasticInput = new StringBuilder
    for (i <- 0 until mixedInput.length) {
      if (i < 2)
        intergalasticInput.append(mixedInput(i) + "#")

    }
    intergalasticInput.toString()
  }

  def storeMissingElementValueInMap(splittedInput: Array[String], missingElementValue: Double, inputProcessor: InputProcessor): Unit = {
    inputProcessor.missingElementValues = inputProcessor.missingElementValues + (extractElementFrom(splittedInput) -> missingElementValue.toString)
  }

  def extractElementFrom(splittedInput: Array[String]): String = {
    splittedInput(2)
  }

  def readSequenceOfQuestionsAndCalculateAnswer(inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal) = inputProcessor.sequenceOfQuestions.map(question => {
    processIndividual(question, romanToDecimal, inputProcessor)
  })

  def processIndividual(question: Map[String, String], romanToDecimal: RomanToDecimal, inputProcessor: InputProcessor): Iterable[Unit] = question.map(tuple
  => {
    val lengthOfQuestion: Int = calculateLengthOfQuestion(tuple._1)
    lengthOfQuestion match {
      case 2 => displayMessageForInvalidQuery()
      case 3 =>
        val (question: Array[String], answer: Double) = calculateHowManyCreditsForIndividualMapping(tuple, inputProcessor, romanToDecimal)
        printFormattedOutput(question, answer)
      case 4 =>
        val (question: Array[String], answer: Double) = calculateHowMuchIsTheValueForIndividualMapping(tuple, romanToDecimal, inputProcessor)
        printFormattedOutput(question, answer)
    }
  })

  def displayMessageForInvalidQuery() = println("I have no idea what you are talking about")

  def calculateLengthOfQuestion(s: String): Int = {
    val splittedInput = splitWith(s, " ")
    splittedInput.length
  }

  def splitWith(mixedInput: String, splitCriteria: String): Array[String] = {
    mixedInput.split(splitCriteria)
  }

  def calculateHowMuchIsTheValueForIndividualMapping(mapping: (String, String), romanToDecimal: RomanToDecimal, inputProcessor: InputProcessor): (Array[String], Double) = {
    val question = extractQuestion(mapping._1).split(" ")
    val romanInput: String = getRomanInputFromCollectionOfIntergalasticUnits(question, inputProcessor)
    val answer = romanToDecimal.convertRomanToDecimal(romanInput.toString)
    (question, answer)
  }

  def getRomanInputFromCollectionOfIntergalasticUnits(collectionOfIntergalasticUnits: Array[String], inputProcessor: InputProcessor): String = {
    val romanInput = new StringBuilder
    collectionOfIntergalasticUnits.map(
      interGalasticUnit => {
        romanInput.append(getRomanEquivalentForGivenIntergalasticInput(interGalasticUnit, inputProcessor))
      })
    romanInput.toString()
  }

  def getRomanEquivalentForGivenIntergalasticInput(interGalasticUnit: String, inputProcessor: InputProcessor): String = {
    inputProcessor.elementToRomanMapping(interGalasticUnit)
  }

  def extractQuestion(tuple: String): String = tuple.substring(0, tuple.length - 1)

  def printFormattedOutput(question: Array[String], answer: Double) = {
    question.map(ques => {
      print(ques + " ")
    })
    print("is " + answer)
    println()
  }

  def calculateHowManyCreditsForIndividualMapping(mapping: (String, String), inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal): (Array[String], Double) = {
    val question = extractQuestion(mapping._1).split(" ")
    val romanInput = new StringBuilder
    var elementValue: Double = 0
    val unit = getRegexForIntergalasticUnit
    val element = getRegexForElementOnEarth
    def mapQuestiontoGetRomanAndElementValue() {
      question.map {
        case unit(unit) =>
          romanInput.append(getRomanEquivalentForGivenIntergalasticInput(unit, inputProcessor))
        case element(element) =>
          elementValue = inputProcessor.missingElementValues(element).toDouble
        case _ =>
      }
    }
    mapQuestiontoGetRomanAndElementValue()
    val decimalValue = romanToDecimal.convertRomanToDecimal(romanInput.toString())
    val answer = decimalValue * elementValue
    (question, answer)
  }

  def getRegexForElementOnEarth: Regex = "([A-Z][a-z]+)".r

  def getRegexForIntergalasticUnit: Regex = "([a-z]+{0,4})".r
}
