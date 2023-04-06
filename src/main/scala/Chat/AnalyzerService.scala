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
    t match
      case Command(num, product, brand) => 
        val brandName = brand.getOrElse(productSvc.getDefaultBrand(product));
        val price = productSvc.getPrice(product, brandName)
        num * price
      case And(left, right) => 
        computePrice(left) * computePrice(right)
      case Or(left, right) => 
        computePrice(left) min computePrice(right) // return the minimum
      case _ =>
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
            if !accountSvc.isAccountExisting(user) then
              "Vous n'avez pas de compte !"
            else
              val solde = accountSvc.getAccountBalance(user)
              "Vous avez "+ solde +" CHF sur votre compte."
          case None => 
            "Je ne sais pas qui vous êtes !"
        
      case AskPrice(command) => "Le prix de votre commande est de "+ computePrice(command) +" CHF."
      case Buy(command) => 
        session.getCurrentUser match
          case Some(user) =>
            if !accountSvc.isAccountExisting(user) then
              "Vous n'avez pas de compte !"
            else
              val price = computePrice(command)
              if accountSvc.getAccountBalance(user) < price then
                "Vous n'avez pas assez d'argent sur votre compte !"
              else
                accountSvc.purchase(user, price)
                "Voilà votre commande !"
          case None => 
            "Je ne sais pas qui vous êtes !"
      case Login(user) => 
        session.setCurrentUser(user)
        "Bonjour " + user + " !"
        // TODO : création du compte ?
      case _ => "Je ne comprends pas votre demande."
end AnalyzerService
