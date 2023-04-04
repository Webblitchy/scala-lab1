package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  def word2tuple(w:String) : (String,Token) = 
    val normalized_word = spellCheckerSvc.getClosestWordInDictionary(w)
    //returns the tuple (normalized_word, token)
    (normalized_word, normalized_word match
      case "bonjour" => Token.BONJOUR
      case "je" => Token.JE
      case "etre" => Token.ETRE
      case "vouloir" => Token.VOULOIR
      case "assoiffe" => Token.ASSOIFFE
      case "affame" =>  Token.AFFAME
      case "biere" => Token.PRODUIT
      case "croissant" =>  Token.PRODUIT
      case "et" => Token.ET
      case "ou" => Token.OU
      case "svp" => Token.SVP
      case w if w.matches("[0-9]+") => Token.NUM
      //if the word starts with _ it is a pseudo
      case w if w.startsWith("_") => Token.PSEUDO
      case _ => Token.UNKNOWN
      )
    
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
  // DONE - Part 1 Step 3
  // TODO - Part 2 Step 1
  def tokenize(input: String): Tokenized = 
    TokenizedImpl(sanitize(input).split(" ").map(w => word2tuple(w)).toArray)
end TokenizerService
