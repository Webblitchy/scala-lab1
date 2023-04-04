package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  def word2tuple(w:String) : (String,Token) = 
    val normalized_word = spellCheckerSvc.getClosestWordInDictionary(w)
    //returns the tuple (normalized_word, token)
    (normalized_word, normalized_word match
      // Interjections
      case "bonjour" => Token.BONJOUR
      case "svp" => Token.SVP
      // Questions
      case "quel" => Token.QUEL
      case "combien" => Token.COMBIEN
      // Articles
      case "je" => Token.JE
      case "me" => Token.ME
      case "le" => Token.LE
      case "mon" => Token.MON
      case "de" => Token.DE

      // Adjectives
      case "assoiffe" => Token.ASSOIFFE
      case "affame" =>  Token.AFFAME

      // Actions
      case "etre" => Token.ETRE
      case "vouloir" => Token.VOULOIR
      case "couter" => Token.COUTER
      case "commander" => Token.COMMANDER
      case "appeler" => Token.APPELER

      // Logic Operators
      case "et" => Token.ET
      case "ou" => Token.OU
      // Products
      case "biere" => Token.PRODUIT
      case "croissant" =>  Token.PRODUIT
      case "maison" => Token.MARQUE
      case "cailler" => Token.MARQUE
      case "farmer" => Token.MARQUE
      case "boxer" => Token.MARQUE
      case "wittekop" => Token.MARQUE
      case "punkipa" => Token.MARQUE
      case "jackhammer" => Token.MARQUE
      case "tenebreuse" => Token.MARQUE
      case "solde" => Token.SOLDE
      case "prix" => Token.PRIX
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
  // DONE - Part 2 Step 1
  def tokenize(input: String): Tokenized = 
    TokenizedImpl(sanitize(input).split(" ").map(w => word2tuple(w)).toArray)
end TokenizerService
