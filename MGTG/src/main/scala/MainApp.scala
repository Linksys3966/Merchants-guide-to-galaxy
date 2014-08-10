object MainApp {

  def main(args: Array[String]) {
    val (inputprocessor: InputProcessor, romanToDecimal: RomanToDecimal, outputProcessor: OutputProcessor) = factoryForCreatingObjects
    addRomanToDecimalMappings(romanToDecimal)
    readInputFromFileAndStoreMappings(outputProcessor)
    calculateMissingValuesForMetals(outputProcessor)
    readSequenceOfQuestionsAndCalculateAnswer(outputProcessor)

  }

  def readSequenceOfQuestionsAndCalculateAnswer(outputProcessor: OutputProcessor) {
    outputProcessor.readSequenceOfQuestionsAndCalculateAnswer()
  }

  def factoryForCreatingObjects: (InputProcessor, RomanToDecimal, OutputProcessor) = {
    val inputprocessor = new InputProcessor()
    val romanToDecimal = new RomanToDecimal()
    val outputProcessor = new OutputProcessor(inputprocessor, romanToDecimal)
    (inputprocessor, romanToDecimal, outputProcessor)
  }

  def addRomanToDecimalMappings(romanToDecimal: RomanToDecimal) {
    romanToDecimal.addRomanToDecimalMappings("I", 1)
    romanToDecimal.addRomanToDecimalMappings("V", 5)
    romanToDecimal.addRomanToDecimalMappings("X", 10)
    romanToDecimal.addRomanToDecimalMappings("L", 50)
    romanToDecimal.addRomanToDecimalMappings("C", 100)
    romanToDecimal.addRomanToDecimalMappings("D", 500)
    romanToDecimal.addRomanToDecimalMappings("M", 1000)
  }

  def readInputFromFileAndStoreMappings(outputProcessor: OutputProcessor) {
    outputProcessor.readFileAndStoreMappings()
  }

  def calculateMissingValuesForMetals(outputProcessor: OutputProcessor) {
    outputProcessor.calculateMissingValuesForAllMappings()
  }
}
