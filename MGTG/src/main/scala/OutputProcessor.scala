import scala.util.matching.Regex

class OutputProcessor(inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal) {

  def readFile() {
    inputProcessor.readFromFile("/Users/vivekpatil/Data.txt")
  }

  def calculateMissingValues() = {
    val mappings = inputProcessor.mixedToCreditsMapping
    mappings.map(mappingTuple => {
      val mixedInput = mappingTuple._1
      val credits = mappingTuple._2.toDouble
      val splittedInput = splitWith(mixedInput, " ")
      val intergalsaticInputs = getIntergalasticInput(splittedInput)
      val collectionOfIntergalasticUnits = splitWith(intergalsaticInputs, "#")
      val romanInput: String = getRomanInputFromCollectionOfIntergalasticUnits(collectionOfIntergalasticUnits)
      val decimalValue = romanToDecimal.convertRomanToDecimal(romanInput)
      val missingElementValue: Double = calculateMissingValueOfElement(credits, decimalValue)
      storeMissingElementValueInMap(splittedInput, missingElementValue)
    })
  }

  def storeMissingElementValueInMap(splittedInput: Array[String], missingElementValue: Double): Option[String] = {
    inputProcessor.missingElementValues.put(getElement(splittedInput), missingElementValue.toString)
  }

  def getElement(splittedInput: Array[String]): String = {
    splittedInput(2)
  }

  def calculateMissingValueOfElement(credits: Double, decimalValue: Int): Double = {
    val missingElementValue: Double = credits / decimalValue
    missingElementValue
  }

  def getRomanInputFromCollectionOfIntergalasticUnits(collectionOfIntergalasticUnits: Array[String]): String = {
    val romanInput = new StringBuilder
    collectionOfIntergalasticUnits.map(
      interGalasticUnit => {
        romanInput.append(inputProcessor.elementToRomanMapping(interGalasticUnit))
      })
    romanInput.toString()
  }

  def splitWith(mixedInput: String, splitCriteria: String): Array[String] = {
    mixedInput.split(splitCriteria)
  }

  def getIntergalasticInput(mixedInput: Array[String]): String = {
    val intergalasticInput = new StringBuilder
    for (i <- 0 until mixedInput.length) {
      if (i < 2)
        intergalasticInput.append(mixedInput(i) + "#")

    }
    intergalasticInput.toString()
  }

  def combineIntergalasticInputsToOneString(intergalasticInputs: Array[String]): String = {
    val combinedString = new StringBuilder
    intergalasticInputs.map(intergalasticInput => {
      combinedString.append(intergalasticInput)
    })
    combinedString.toString()
  }

  def calculatehowMuchisValue() = {
    val mappings = inputProcessor.outputValueOfUnits
    mappings.map(mapping => {
      val question = extractQuestion(mapping._1).split(" ")
      val romanInput: String = getRomanInputFromCollectionOfIntergalasticUnits(question)
      val answer = romanToDecimal.convertRomanToDecimal(romanInput.toString())
      printFormattedOutput(question, answer)
    })
  }

  def printFormattedOutput(question: Array[String], answer: Double) = {
    question.map(ques => {
      print(ques + " ")
    })
    print(answer)
    println()
  }

  def extractQuestion(tuple: String): String = tuple.substring(0, tuple.length - 1)

  def calculatehowManyCredits() = {
    val mappings = inputProcessor.outputValueOfCredits
    val unit = getRegexForIntergalasticUnit
    val element = getRegexForElementOnEarth
    mappings.map(mapping => {
      val question = extractQuestion(mapping._1).split(" ")
      val romanInput = new StringBuilder
      var elementValue: Double = 0
      def mapQuestiontoGetRomanAndElementValue {
        question.map {
          case unit(unit) =>
            romanInput.append(inputProcessor.elementToRomanMapping(unit))
          case element(element) =>
            elementValue = inputProcessor.missingElementValues(element).toDouble
          case _ =>
        }
      }
      mapQuestiontoGetRomanAndElementValue
      val decimalValue = romanToDecimal.convertRomanToDecimal(romanInput.toString())
      val answer = decimalValue * elementValue
      printFormattedOutput(question, answer)
    })
  }

  def getRegexForElementOnEarth: Regex = {
    "([A-Z][a-z]+)".r
  }

  def getRegexForIntergalasticUnit: Regex = {
    "([a-z]+{0,4})".r
  }

}
