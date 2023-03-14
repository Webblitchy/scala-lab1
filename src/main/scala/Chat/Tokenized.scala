package Chat

import Chat.Token.*
import Utils.SpellCheckerService

trait Tokenized:
  /**
    * Get the next token of the user input, or EOL if there is no more token.
    * @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token)

class TokenizedImpl(val tokens: Array[(String, Token)]) extends Tokenized:
  // DONE - Part 1 Step 3
  var index = 0
  def nextToken(): (String, Token) = 
    if tokens.length  == index then return ("EOL", Token.EOL)
    else
      index += 1
      return tokens(index -1)   
  
end TokenizedImpl
