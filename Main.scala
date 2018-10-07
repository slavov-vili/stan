//package test

import trie.SearchTrie
import trie.TrieIO
import ngrammer.NgramMaker
import java.io.File
import scala.io.Source

object Main {

  def main(args: Array[String]) {
    val dirName = "wordLemPos"
    println("constructing the trie")
    // the trie of words
    val trie = new SearchTrie
    println("creating n-grams")

    // for each file in the directory
    // add all words to the trie
    // extract all Ngrams from the files
    for(filepath <- new File(dirName).listFiles
                                     .filter(_.getName
                                              .endsWith(".txt"))) {
      TrieIO.readFile(trie, filepath)
      NgramMaker.readFile(filepath)
    } //end for

    // count all the words in the texts
    val totalWC = NgramMaker.unigramCount
        .map{case (_,count) => count}.reduce(_ + _)

    // read Stdin and respond
    println("Enter a sentence:")
    read_std_in(trie, totalWC)
  } //end main



  // reads from Stdin and responds to querries
  // (corrects misspelled words)
  def read_std_in(myTrie: trie.SearchTrie, totalWC: Int): Unit = {
    // for each line in the input
    for(ln <- Source.stdin.getLines) {
      // split the input into a list of lowercased words
      // and print the corrected sentence
      println(correct_sentence(ln.split(" ").map(x => x.toLowerCase),
                               myTrie, totalWC))
    } //end for
  } //end def



  // corrects all mistakes in the input sentence and returns the correct one
  def correct_sentence(sent: Array[String], myTrie: trie.SearchTrie,
                       totalWC: Int): String = {
    // while there are mistakes in the sentence
    while (sent.exists(x => !myTrie.contains(x))) {
      // get the index of the first misspelled word in the sentence
      val i = sent.indexWhere(!myTrie.contains(_))
      // store the word itself
      val word = sent(i)

      // get all words, close to the misspelled word
      // and their distance from that word
      val withinX = get_within_x(myTrie, word)
      //withinX.foreach(println)
      //withinX.foreach{case (w,d) => println(s"$w, $d, ${NgramMaker.unigramCount(w)}")}
      val wordScores = withinX.map(x => calc_word_score(sent, i, x, totalWC))
      wordScores.foreach(println)
      sent(i) = wordScores.maxBy(_._2)._1
      //wordScores.filter(_ == wordScores.maxBy(_._2)).foreach(println)
    }  //end while

    // return the sentence
    sent.mkString(" ")
  } //end def



  // gets all words within edit distance X of the given word
  def get_within_x(theTrie: trie.SearchTrie,
                    word: String): List[(String, Int)] = {
    // first search for words within edit distance 2
    var n = 2
    var within = theTrie.search(word, n)
    // if there are no such words
    while (within.isEmpty && n < (word.length / 3)) {
      n += 1
      within = theTrie.search(word, n)
    } //end if

    within.toList//.map{case (w,d) => (w,d.toDouble)}//.map{case (w,d) => (w, Math.pow(2.0, d) / (w.length + word.length))}
  } //end def



  // calculates the score of the word given the context
  def calc_word_score(sent: Array[String], idx: Int,
                      candidate: (String, Int),
                      totalWC: Int): (String, Double) = {
    //println(candidate)
    val dist = candidate._2
    sent(idx) = candidate._1
    //println(s"index: $idx")
    var sum = 0.0
    // the sum of the previously calculated n-grams
    var prevSum = 1.0

    // for unigrams, bigrams and trigrams
    for (i <- 1 to 3) {
      //println(s"ngram: $i")
      // temporary sum for the counts of each n-gram
      var ngramSum = 0.0

      // for all possible contexts of the word
      for (j <- idx-(i-1) to idx) {
        //println(s"starting index: $j")

        // take the context
        val context = sent.slice(j, j+i)
        //println(context.mkString(" "))

        if (context.length == i) {
          // add the count to the sum
          i match {
            //case 1 => ngramSum += 0
            case 1 => {
                val wc = NgramMaker.unigramCount.getOrElse(context(0), 0)
                ngramSum += Math.sqrt(wc)
                //ngramSum += Math.pow((ngramSum * Math.sqrt(wc)), 2)
            } //end case
            case 2 => {
                val wc = NgramMaker.bigramCount
                         .getOrElse((context(0), context(1)), 0)
                ngramSum += wc
            } //end case
            case 3 => {
                val wc = NgramMaker.trigramCount
                         .getOrElse((context(0), context(1), context(2)), 0)
                ngramSum += wc
            } //end case
          } //end match
        } //end if
      } //end for

       //adjust sum based on n-gram weight and add it to the total sum
      //sum += Math.pow(ngramSum, (1 / dist))
      prevSum = Math.pow((prevSum * ngramSum), i)
      sum += Math.pow((prevSum * ngramSum), i)
    } //end for

    // make a pair of the word and its score
    (sent(idx), (sum / dist))
  } //end def


} //end object
