package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  def word2tok(input:String) : Token = 
    input.toLowerCase match
      case "hello" => Token.BONJOUR
      case "veux" => Token.VOULOIR
    
  def sanitize(input: String): String = 
    input.toLowerCase
      .replaceAll("[^a-z0-9 _]", "") // delete non digit,alphabet,'_'and' '
      .replaceAll(" +", " ")
      .trim
  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  // TODO - Part 1 Step 3
  def tokenize(input: String): Tokenized = 
    TokenizedImpl(input.split(" ").map(w => sanitize(w) match

      case w => (w, word2tok(spellCheckerSvc.getClosestWordInDictionary(w)))
    ).toArray)
    // TokenizedImpl(input.split(" ").map(w => (w,spellCheckerSvc))
end TokenizerService
