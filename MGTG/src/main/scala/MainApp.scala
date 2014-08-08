/**
 * Created by vivekpatil on 8/6/14.
 */
object MainApp {

  def main(args: Array[String])
  {
    val inputprocessor=new InputProcessor()
    val romanToDecimal=new RomanToDecimal()
    romanToDecimal.addRomanToDecimalMappings("I",1)
    romanToDecimal.addRomanToDecimalMappings("V",5)
    romanToDecimal.addRomanToDecimalMappings("X",10)
    romanToDecimal.addRomanToDecimalMappings("L",50)
    romanToDecimal.addRomanToDecimalMappings("C",100)
    romanToDecimal.addRomanToDecimalMappings("D",500)
    romanToDecimal.addRomanToDecimalMappings("M",1000)
    val outputProcessor=new OutputProcessor(inputprocessor,romanToDecimal)
    outputProcessor.readFile()
    outputProcessor.calculateMissingValues()
    outputProcessor.calculatehowMuchisValue()
    outputProcessor.calculatehowManyCredits()

  }

}
