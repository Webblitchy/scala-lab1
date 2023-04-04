package Chat
import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):
  import ExprTree._
  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    * @return the result of the computation
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): Double =
    // TODO
    0.0 

  /**
    * Return the output text of the current node, in order to write it in console.
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    t match
      // TODO - Part 2 Step 3
      case Thirsty => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case AskSold => 
        session.getCurrentUser match
          case Some(user) =>
            "Vous avez N CHF sur votre compte." // TODO
          case None => 
            "Je ne sais pas qui vous êtes !"
        
      case AskPrice(command: ExprTree) => ""
      case Buy(command: ExprTree) => ""
      case Login(user) => 
        session.setCurrentUser(user)
        "Bonjour " + user + " !"
end AnalyzerService
