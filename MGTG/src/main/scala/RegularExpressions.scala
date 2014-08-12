import scala.util.matching.Regex

object RegularExpressions {

  def createRegexForQuestionOfTypeWhichNeedConversionBetweenMetals(): Regex = "many (Platinum|Gold|Silver)".r

  def createRegexForQuestionOfTypeHowMuch(): Regex = "(much is)".r

  def createRegexForQuestionOfTypeHowMany(): Regex = "(many Credits)".r

  def createRegexForInputEndingWithRomanCharacter: Regex = {
    "([IVLXCDM])".r
  }

  def createRegexForInputEndingWithCredits: Regex = "(Credits)".r

  def createRegexForIdentifyingTheQuestion: Regex = "(\\?)".r

  def getRegexForElementOnEarth: Regex = "([A-Z][a-z]+)".r

  def getRegexForIntergalasticUnit: Regex = "([a-z]{4})".r


}
