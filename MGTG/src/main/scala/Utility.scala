/**
 * Created by vivekpatil on 8/12/14.
 */
object Utility {

  def printFormattedOutput(question: Array[String], answer: String) = {
    question.map(ques => {
      print(ques + " ")
    })
    print("is " + answer)
    println()
  }

  def splitWith(mixedInput: String, splitCriteria: String): Array[String] = {
    mixedInput.split(splitCriteria)
  }

  def printFormattedOutputForConversionBetweenMetals(question: Array[String], answer: String) = {
    println(answer + " " + question(0) + " " + question(1) + " " + question(2) + " " + question(3))
  }

  def messageForFileNotFound(fileName: String) = println("File " + fileName + " Not Found")

  def displayMessageForInvalidQuery() = println("I have no idea what you are talking about")

}
