package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg){}

class Parser(tokenized: Tokenized):
  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = tokenized.nextToken()

  /** "Eats" the expected token and returns it value, or terminates with an error. */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type Token */
  private def expected(token: Token, more: Token*): Nothing =
    expected(more.prepended(token))
  private def expected(tokens: Seq[Token]): Nothing =
    val expectedTokens = tokens.mkString(" or ")
    throw new UnexpectedTokenException(s"Expected: $expectedTokens, found: $curToken")

  /** the root method of the parser: parses an entry phrase */
  // DONE - Part 2 Step 4
  def parsePhrases() : ExprTree =
    if curToken == BONJOUR then readToken()
    if curToken == JE then
      readToken() // ignore the "je"
      if curToken == ETRE then
        readToken()
        if curToken == ASSOIFFE then
          readToken()
          Thirsty
        else if curToken == AFFAME then
          readToken()
          Hungry
        else if curToken == PSEUDO then
          val pseudo = eat(PSEUDO).tail // remove the begining _
          Login(pseudo)
        else expected(ASSOIFFE, AFFAME, PSEUDO)
      else if curToken == VOULOIR then
        readToken()
        if curToken == COMMANDER then
          readToken()
          Buy(parseCommand)
        else if curToken == CONNAITRE then
          readToken()
          eat(MON)
          eat(SOLDE)
          AskSold
        else expected(COMMANDER, CONNAITRE)
      else if curToken == ME then
        readToken()
        eat(APPELER)
        Login(curValue)
      else expected(ETRE, VOULOIR, ME)
    else if curToken == COMBIEN then
      readToken()
      eat(COUTER)
      AskPrice(parseCommand)
    else if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
      AskPrice(parseCommand)
    else expected(JE, COMBIEN, QUEL) // pourquoi BONJOUR

  def parseCommand : ExprTree =
    var command : Command = null
    if curToken == NUM then
      val num = curValue.toInt
      readToken()
      if curToken == PRODUIT then
        val product = curValue
        readToken()
        command = Command(num, product, None)
        if curToken == MARQUE then
          val brand = curValue
          readToken()
          command = Command(num, product, Some(brand))
      else expected(PRODUIT)
    else expected(NUM)

    if curToken == ET then
      readToken()
      And(command, parseCommand)
    else if curToken == OU then
      readToken()
      Or(command, parseCommand)
    else 
      command