

import scala.collection.mutable

class RomanToDecimal {

  var romanDecimalPairs = new mutable.HashMap[String, Int]()

  def addRomanToDecimalMappings(roman: String, decimal: Int) = {
    romanDecimalPairs.put(roman, decimal)
  }

  def convertRomanToDecimal(romanString: String): Double = {
    var decimal: Double = 0
    var previousvalue: Double = 0
    var currentvalue: Int = 0

    def checkSubstractionLogic(currentvalue: Int) = {
      if (currentvalue > previousvalue) {
        decimal += currentvalue - (2 * previousvalue)
      }
      else {
        decimal = decimal + currentvalue
      }
      previousvalue = currentvalue
    }

    romanString.map {
      case romanCharacter@'I' =>
        currentvalue = romanDecimalPairs(romanCharacter.toString)
        checkSubstractionLogic(currentvalue)
      case romanCharacter@'V' =>
        currentvalue = romanDecimalPairs(romanCharacter.toString)
        checkSubstractionLogic(currentvalue)
      case romanCharacter@'X' =>
        currentvalue = romanDecimalPairs(romanCharacter.toString)
        checkSubstractionLogic(currentvalue)
      case romanCharacter@'L' =>
        currentvalue = romanDecimalPairs(romanCharacter.toString)
        checkSubstractionLogic(currentvalue)
      case romanCharacter@'C' =>
        currentvalue = romanDecimalPairs(romanCharacter.toString)
        checkSubstractionLogic(currentvalue)
      case romanCharacter@'D' =>
        currentvalue = romanDecimalPairs(romanCharacter.toString)
        checkSubstractionLogic(currentvalue)
      case romanCharacter@'M' =>
        currentvalue = romanDecimalPairs(romanCharacter.toString)
        checkSubstractionLogic(currentvalue)
      case _ => println("Please Enter a Valid Roman Input")
    }
    decimal
  }
}
