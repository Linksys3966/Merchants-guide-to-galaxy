
object Extractor {
  def extractRomanAndElementValue(inputProcessor: InputProcessor, question: Array[String]): (StringBuilder, Double) = {
    val romanInput = new StringBuilder
    var elementValue: Double = 0
    val unit = RegularExpressions.getRegexForIntergalasticUnit
    val element = RegularExpressions.getRegexForElementOnEarth
    question.map {
      case unit(unit) =>
        romanInput.append(getRomanEquivalentForGivenIntergalasticInput(unit, inputProcessor))
      case element(element) =>
        elementValue = inputProcessor.missingElementValues(element).toDouble
      case _ =>
    }
    (romanInput, elementValue)
  }

  def extractQuestion(tuple: String): String = tuple.substring(0, tuple.length - 1)

  def extractElementFrom(splittedInput: Array[String]): String = {
    splittedInput(2)
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

  def getIntergalasticInputFrom(mixedInput: Array[String]): String = {
    val intergalasticInput = new StringBuilder
    for (i <- 0 until mixedInput.length) {
      if (i < 2)
        intergalasticInput.append(mixedInput(i) + "#")
    }
    intergalasticInput.toString()
  }

  def extractThePartUsedForDecidingTheTypeOfQuestionFrom(splittedInput: Array[String]): String = splittedInput(1) + " " + splittedInput(2)
}
