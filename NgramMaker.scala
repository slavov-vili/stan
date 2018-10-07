package ngrammer

import java.io.File
import java.nio.charset.CodingErrorAction
import scala.io.Codec
import scala.io.Source
import scala.collection.mutable.HashMap

object NgramMaker {

  val unigramCount = HashMap[String, Int]()
  val bigramCount = HashMap[(String, String), Int]()
  val trigramCount = HashMap[(String, String, String), Int]()

  def main(args: Array[String]): Unit = {
    val path = getClass.getResource("wordLemPos").getPath
    readFiles(path)
    val arr1 = Array("run", "runs", "turn", "turns")
    val arr2 = Array(("he", "run"),
                     ("he", "runs"),
                     ("he", "turns"),
                     ("i", "run"),
                     ("i", "runs"),
                     ("i", "turn"),
                     ("i", "turns"),
                     ("run", "away"),
                     ("runs", "away"),
                     ("turn", "away"),
                     ("turns", "away"))
    val arr3 = Array(("he", "run", "away"),
                     ("he", "runs", "away"),
                     ("he", "turns", "away"),
                     ("i", "run", "away"),
                     ("i", "turn", "away"))
    arr1.foreach(x => println(s"$x: ${unigramCount.getOrElse(x, 0)}"))
    arr2.foreach(x => println(s"$x: ${bigramCount.getOrElse(x, 0)}"))
    arr3.foreach(x => println(s"$x: ${trigramCount.getOrElse(x, 0)}"))
    //unigramCount.foreach(println)
    //bigramCount.foreach(println)
    //trigramCount.foreach(println)
    //println(unigramCount)
    //println(trigramCount)
  }   //end main



  def readFiles(filepath: String): Unit = {
    // get all the files from the filepath
    val dir = new File(filepath)
        //println("counting word frequency")
        //println("counting bigram frequency")
        //println("counting trigram frequency")
    val files = dir.listFiles.filter(_.getName.endsWith(".txt")).toList
    // read each file
    files.foreach(readFile)
  } //end def



  def readFile(file: File): Unit = {
    //println(s"reading: ${file.getName()}")
    // detect and replace illegal UTF-8 byte sequences
    // (needed for some of our input files)
    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    // make an iterator over all the words in each line
    val wordIter = Source.fromFile(file)
                         .getLines
                         .map(x => takeWord(x))
    var lastWord = ""
    var sent = List[String]()
    // for each word
    for(word <- wordIter) {
      // if at the end of a sentence
      if(isEOS(lastWord) && word.matches("\\b[A-Z].*")) {
        // get the separate sentences without punctuation
        val cleanSent = sent.filter(_.matches("[A-Za-z0-9].*"))
                           .reverse
                           .map(_.toLowerCase)
        cleanSent.foreach(x => addToMap(x, unigramCount))
        //println(cleanSent.mkString(" "))

        // extract all bigrams from the sentence
        val newBigrams = cleanSent.sliding(2).toList
          .collect{case List(x,y) => (x,y)}
          //newBigrams.foreach(println)
        newBigrams.foreach(addToMap(_, bigramCount))
        //println(newBigrams)

        // extract all the trigrams from the sentence
        val newTrigrams = cleanSent.sliding(3).toList
          .collect{case List(x,y,z) => (x,y,z)}
        newTrigrams.foreach(addToMap(_, trigramCount))
        //println(newTrigrams)

        sent = List(word)
      } //end if
      else
        sent = word :: sent

      lastWord = word
    } //end for
  } //end def



  // adds a key to a hashmap
  def addToMap[T](key: T, map: HashMap[T, Int]): Unit = {
  if(map.contains(key))
     map(key) += 1
   else
     map(key) = 1
  } //end def



  // checks if a string represents an EndOfSentence symbol
  def isEOS(str: String): Boolean = {
    val res = if(str == "." || str == "!" || str == "?")
                true
              else
                false
    res
  } //end def



  // extracts the word from line
  def takeWord(line: String): String = {
    val parts = line.split("\t")
    val res = if (parts.isDefinedAt(0))
                parts(0)
              else
                ""
    res
  } //end def

} //end object
