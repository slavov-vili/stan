object Tokenizer {

  def tokenize(text: String): Array[String] = {
    text.split("\\s+").filter(_.matches("[A-Za-z0-9]+"))
  }

  def main(args: Array[String]): Unit = {
    val str = "asdf asldjk4 9sf8u49 84 294u9  9248u &4242o fslj& sldkjf ( ldkj)"

    val res = tokenize(str)

    for(s <- res) {
      println(s)
    }
  }

}
