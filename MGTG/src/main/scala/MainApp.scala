object MainApp {

  def main(args: Array[String]) {
    val (inputprocessor: InputProcessor, romanToDecimal: RomanToDecimal, outputProcessor: OutputProcessor) = factoryForCreatingObjects
    addRomanToDecimalMappings(romanToDecimal)
    readInputFromFileAndStoreMappings(inputprocessor)
    calculateMissingValuesForMetals(outputProcessor, inputprocessor, romanToDecimal)
    readSequenceOfQuestionsAndCalculateAnswer(outputProcessor, inputprocessor, romanToDecimal)
  }

  def readSequenceOfQuestionsAndCalculateAnswer(outputProcessor: OutputProcessor, inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal) {
    outputProcessor.readSequenceOfQuestionsAndCalculateAnswer(inputProcessor, romanToDecimal)
  }

  def factoryForCreatingObjects: (InputProcessor, RomanToDecimal, OutputProcessor) = {
    val inputprocessor = new InputProcessor()
    val romanToDecimal = new RomanToDecimal()
    val outputProcessor = new OutputProcessor()
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

  def readInputFromFileAndStoreMappings(inputProcessor: InputProcessor) {
    inputProcessor.readDataFromFileAndStoreMappings("/Users/vivekpatil/Data.txt")
  }

  def calculateMissingValuesForMetals(outputProcessor: OutputProcessor, inputProcessor: InputProcessor, romanToDecimal: RomanToDecimal) {
    outputProcessor.calculateMissingValuesForAllMappings(inputProcessor, romanToDecimal)
  }
}
