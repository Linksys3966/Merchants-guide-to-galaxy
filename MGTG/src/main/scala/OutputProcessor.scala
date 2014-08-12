

class OutputProcessor() {

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
    val splittedInput = Utility.splitWith(mixedInput, " ")
    val intergalsaticInputs = Extractor.getIntergalasticInputFrom(splittedInput)
    val collectionOfIntergalasticUnits = Utility.splitWith(intergalsaticInputs, "#")
    val romanInput: String = Extractor.getRomanInputFromCollectionOfIntergalasticUnits(collectionOfIntergalasticUnits, inputProcessor)
    val decimalValue = romanToDecimal.convertRomanToDecimal(romanInput)
    val missingElementValue: Double = logicForComputingMissingValue(credits, decimalValue)
    (splittedInput, missingElementValue)
  }

  def logicForComputingMissingValue(credits: Double, decimalValue: Double): Double = {
    val missingElementValue: Double = credits / decimalValue
    missingElementValue
  }

  def storeMissingElementValueInMap(splittedInput: Array[String], missingElementValue: Double, inputProcessor: InputProcessor): Unit = {
    inputProcessor.missingElementValues = inputProcessor.missingElementValues + (Extractor.extractElementFrom(splittedInput) -> missingElementValue.toString)
  }

  def readSequenceOfQuestionsAndCalculateAnswer(inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal) = inputProcessor.sequenceOfQuestions.map(question => {
    processIndividual(question, romanToDecimal, inputProcessor)
  })

  def processIndividual(question: Map[String, String], romanToDecimal: RomanToDecimal, inputProcessor: InputProcessor): Iterable[Unit] = question.map(tuple
  => {
    val lengthOfQuestion: Int = calculateLengthOfQuestion(tuple._1)
    lengthOfQuestion match {
      case 2 => Utility.displayMessageForInvalidQuery()
      case 3 =>
        val (question: Array[String], answer: Double) = calculateConversionBetweenMetals(tuple, inputProcessor, romanToDecimal)
        Utility.printFormattedOutputForConversionBetweenMetals(question, answer.toString)
      case 4 =>
        val (question: Array[String], answer: Double) = calculateHowManyCreditsForIndividualMapping(tuple, inputProcessor, romanToDecimal)
        Utility.printFormattedOutput(question, answer.toInt.toString + " Credits")
      case 5 =>
        val (question: Array[String], answer: Double) = calculateHowMuchIsTheValueForIndividualMapping(tuple, romanToDecimal, inputProcessor)
        Utility.printFormattedOutput(question, answer.toInt.toString)
    }
  })

  def calculateConversionBetweenMetals(mapping: (String, String), inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal): (Array[String], Double) = {
    val question = Extractor.extractQuestion(mapping._1).split(" ")
    val valueToBeConverted: Double = inputProcessor.missingElementValues(question(0)).toDouble
    val (romanInput: StringBuilder, elementValue: Double) = Extractor.extractRomanAndElementValue(inputProcessor, question)
    val decimalValue = romanToDecimal.convertRomanToDecimal(romanInput.toString())
    val answer: Double = logicForComputingConversion(valueToBeConverted, elementValue, decimalValue)
    (question, answer)
  }

  def logicForComputingConversion(valueToBeConverted: Double, elementValue: Double, decimalValue: Double): Double = {
    logicForComputingCredits(elementValue, decimalValue) / valueToBeConverted
  }

  def logicForComputingCredits(elementValue: Double, decimalValue: Double): Double = {
    decimalValue * elementValue
  }

  def calculateLengthOfQuestion(question: String): Int = if (question.contains(" is ")) 3
  else {
    val splittedInput = Utility.splitWith(question, " ")
    splittedInput.length
  }

  def calculateHowMuchIsTheValueForIndividualMapping(mapping: (String, String), romanToDecimal: RomanToDecimal, inputProcessor: InputProcessor): (Array[String], Double) = {
    val question = Extractor.extractQuestion(mapping._1).split(" ")
    val romanInput: String = Extractor.getRomanInputFromCollectionOfIntergalasticUnits(question, inputProcessor)
    val answer = romanToDecimal.convertRomanToDecimal(romanInput.toString)
    (question, answer)
  }

  def calculateHowManyCreditsForIndividualMapping(mapping: (String, String), inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal): (Array[String], Double) = {

    val question = Extractor.extractQuestion(mapping._1).split(" ")
    val (romanInput: StringBuilder, elementValue: Double) = Extractor.extractRomanAndElementValue(inputProcessor, question)
    val decimalValue = romanToDecimal.convertRomanToDecimal(romanInput.toString())
    val answer = logicForComputingCredits(elementValue, decimalValue)
    (question, answer)
  }
}
