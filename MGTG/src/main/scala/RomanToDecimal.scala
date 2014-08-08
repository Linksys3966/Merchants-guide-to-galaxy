

import scala.collection.mutable

class RomanToDecimal {

  var romanDecimalPairs = new mutable.HashMap[String, Int]()

  def addRomanToDecimalMappings(roman: String, decimal: Int) = {
    romanDecimalPairs.put(roman, decimal)
  }

  def convertRomanToDecimal(romanString: String): Int = {
    var decimal: Int = 0
    var previousvalue: Int = 0
    romanString.map(romanCharacter => {
      romanCharacter match {
        case 'I' => {
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          if (currentvalue > previousvalue) {
            decimal += currentvalue - (2 * previousvalue)
          }
          else {
            decimal = decimal + currentvalue
          }
          previousvalue = currentvalue
        }
        case 'V' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          if (currentvalue > previousvalue) {
            decimal += currentvalue - (2 * previousvalue)
          }
          else {
            decimal += currentvalue
          }
          previousvalue = currentvalue
        case 'X' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          if (currentvalue > previousvalue) {
            decimal += currentvalue - (2 * previousvalue)
          }
          else {
            decimal += currentvalue
          }
          previousvalue = currentvalue
        case 'L' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          if (currentvalue > previousvalue) {
            decimal += currentvalue - (2 * previousvalue)
          }
          else {
            decimal += currentvalue
          }
          previousvalue = currentvalue
        case 'C' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          if (currentvalue > previousvalue) {
            decimal += currentvalue - (2 * previousvalue)
          }
          else {
            decimal += currentvalue
          }
          previousvalue = currentvalue
        case 'D' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          if (currentvalue > previousvalue) {
            decimal += previousvalue - (2 * currentvalue)
          }
          else {
            decimal += currentvalue
          }
          previousvalue = currentvalue
        case 'M' =>
          val currentvalue = romanDecimalPairs(romanCharacter.toString)
          if (currentvalue > previousvalue) {
            decimal += currentvalue - (2 * previousvalue)
          }
          else {
            decimal += currentvalue
          }
          previousvalue = currentvalue
      }
    })
    decimal
  }
}
