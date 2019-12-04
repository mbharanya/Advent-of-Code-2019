object Day4 extends App {


  class Password(input: Int) {
    private val inputList = input.toString.toCharArray.toList

    def isValid(): Boolean = {
      neverDecreases() && hasDoubles()
    }

    def isValidPart2(): Boolean = {
      neverDecreases() && hasDoubles2()
    }

    private def neverDecreases(): Boolean = inputList.sorted == inputList

    private def hasDoubles(): Boolean = inputList.groupBy(identity).map(_._2.size).max >= 2

    private def hasDoubles2(): Boolean = inputList.groupBy(identity).map(_._2.size).exists(_ == 2)
  }

  assert(new Password(122345).isValid)
  assert(new Password(111111).isValid)
  assert(!new Password(223450).isValid)
  assert(!new Password(123789).isValid)

 val amountValid = (356261 to 846303).map(new Password(_).isValid()).count(_ == true)
  println(s"Part 1: Amount valid ${amountValid}")


  assert(!new Password(123444).isValidPart2)
  assert(new Password(111122).isValidPart2)


  val amountValid2 = (356261 to 846303).map(new Password(_).isValidPart2()).count(_ == true)

  println(s"Part 2: Amount valid ${amountValid2}")

}
