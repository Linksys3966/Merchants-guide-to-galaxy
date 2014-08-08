

import scala.collection.mutable

class RomanToDecimal {

  var romanDecimalPairs = new mutable.HashMap[String, Int]()

  def addRomanToDecimalMappings(roman: String, decimal: Int) = {
    romanDecimalPairs.put(roman, decimal)
  }

  def convertRomanToDecimal(romanString: String): Int = {
    var decimal: Int = 0
    var previousvalue: Int = 0

    def checkSubstractionLogic(currentvalue: Int) = {
      if (currentvalue > previousvalue) {
        decimal += currentvalue - (2 * previousvalue)
      }
      else {
        decimal = decimal + currentvalue
      }
      previousvalue = currentvalue
    }


    romanString.map(romanCharacter => {
      romanCharacter match {
        case 'I' => {
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          checkSubstractionLogic(currentvalue)
        }
        case 'V' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          checkSubstractionLogic(currentvalue)
        case 'X' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          checkSubstractionLogic(currentvalue)
        case 'L' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          checkSubstractionLogic(currentvalue)
        case 'C' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          checkSubstractionLogic(currentvalue)
        case 'D' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          checkSubstractionLogic(currentvalue)
        case 'M' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          checkSubstractionLogic(currentvalue)
        case _ => println("Please Enter a Valid Roman Input")
      }
    })
    decimal
  }
}
