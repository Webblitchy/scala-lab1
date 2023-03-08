package Utils

trait SpellCheckerService:
  /**
    * This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
    * we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /**
    * Calculate the Levenstein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenstein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number or a pseudonym, this function just returns it.
    * @param misspelledWord the mispelled word to correct
    * @return the closest normalized word from "mispelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String]) extends SpellCheckerService:
  // DONE - Part 1 Step 2
  def stringDistance(s1: String, s2: String): Int = 
    (s1,s2) match 
      case ("",s2) => s2.length
      case (s1,"") => s1.length
      case (s1,s2) => 
        val cost = if s1.last == s2.last then 0 else 1
        List(
          stringDistance(s1.init, s2) + 1,
          stringDistance(s1, s2.init) + 1,
          stringDistance(s1.init, s2.init) + cost
        ).min

  // DONE - Part 1 Step 2
  def getClosestWordInDictionary(misspelledWord: String): String = 
    misspelledWord match 
      case d if d.forall(_.isDigit) => d // number
      case s if s(0) == '_' => s // pseudonym
      case s => 
        val closestWord = dictionary.keys.minBy(stringDistance(s,_))
        dictionary(closestWord)

end SpellCheckerImpl
