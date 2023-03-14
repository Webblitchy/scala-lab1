package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  def word2tuple(w:String) : (String,Token) = 
    val normalized_word = spellCheckerSvc.getClosestWordInDictionary(w)
    normalized_word match
      case "bonjour" => (normalized_word,Token.BONJOUR)
      case "je" => (normalized_word,Token.JE)
      case "etre" => (normalized_word,Token.ETRE)
      case "vouloir" => (normalized_word,Token.VOULOIR)
      case "assoiffe" => (normalized_word,Token.ASSOIFFE)
      case "affame" =>  (normalized_word,Token.AFFAME)
      case "biere" =>  (normalized_word,Token.PRODUIT)
      case "croissant" =>  (normalized_word,Token.PRODUIT)
      case "et" => (normalized_word,Token.ET)
      case "ou" => (normalized_word,Token.OU)
      case "svp" => (normalized_word,Token.SVP)
      case w if w.matches("[0-9]+") => (normalized_word,Token.NUM)
      //if the word starts with _ it is a pseudo
      case w if w.startsWith("_") => (normalized_word,Token.PSEUDO)
      case _ => (normalized_word,Token.UNKNOWN)

    
  def sanitize(input: String): String = 
    input.toLowerCase
      .replaceAll("[.;,!?*]", "") // delete non digit,alphabet,'_'and' '
      .replaceAll("[ ']+", " ")
      .trim
  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  // TODO - Part 1 Step 3
  def tokenize(input: String): Tokenized = 
    TokenizedImpl(sanitize(input).split(" ").map(w => word2tuple(w)).toArray)
end TokenizerService
