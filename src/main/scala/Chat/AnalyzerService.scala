package Chat
import Data.{AccountService, ProductService, Session}
import Vector._
import scala.quoted.Expr

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):
  import ExprTree._

//   /*
//    * Compute the string representation of the current node and the next, then returns it.
//    */
  def stringify(t: ExprTree): String =
    t match
      case Command(1, product, brand) => s"1 $product ${brand.getOrElse("")}"
      case Command(n, product, brand) =>
        s"$n ${product}s ${brand.getOrElse("")}"
      case And(left, right) =>
        s"${stringify(left)} et ${stringify(right)}"
      case Or(left, right) =>
        if computePrice(left) > computePrice(right) then s"${stringify(right)}"
        else s"${stringify(left)}"
      case _ => { println("i don't know how we ended up here 2"); "ERROR " }
      
  /**
    * Simplify the Expression Tree by removing useless nodes that will not be part
    * of the result
    * @param t the tree to simplify
    * @return the simplified tree
    */
  def simplifyTree(t: ExprTree) : ExprTree =
    t match
      case Command(num, product, brand) => Command(num,product,brand)
      case And(left, right) => And(simplifyTree(left), simplifyTree(right))
      case Or(l,r) => if computePrice(l) > computePrice(r) then simplifyTree(r) else simplifyTree(l)
      case unexpected => throw new IllegalArgumentException(s"Expected a Command, got $unexpected")
    
  /*
    * Compute the list of items in the Expression Tree 
    * @param t the tree to comput the list for
  */
  def list_of_item(t:ExprTree):List[(Int,String,Command)] = 
    def flatten(inner_t : ExprTree, acc : List[(Int,String,Command)]):List[(Int,String,Command)] = 
      inner_t match
        case Buy(command) => list_of_item(command)
        case AskPrice(command) => list_of_item(command)
        case Or(left, right) => flatten(left,acc) ++ flatten(right,acc)
        case And(left, right) => flatten(left,acc) ++ flatten(right,acc)
        //we compute a hash using the product and the brand
        case Command(num, product, brand) => (num,s"$product ${brand.getOrElse(productSvc.getDefaultBrand(product))}", ExprTree.Command(num, product, brand)) :: acc
        case _ => throw new IllegalArgumentException(s"Expected a Command, got $inner_t")
    flatten(t,Nil)
    
  def computePrice(t : ExprTree) : Double = 
    t match
      case And(left, right) => computePrice(left) + computePrice(right)
      case Or(left, right) =>
        if computePrice(left) < computePrice(right) then computePrice(left)
        else computePrice(right)
      case Command(quantity, productType, brand) =>
        productSvc.getPrice(
          productType,
          // Si la marque n'est pas spécifiée, on récupère celle par défaut
          brand.getOrElse(productSvc.getDefaultBrand(productType))
        ) * quantity
      case unexpected => throw new IllegalArgumentException(s"Expected a Command, got $unexpected")

  /** Return the output text of the current node, in order to write it in
    * console.
    * @return
    *   the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    t match
      // DONE - Part 2 Step 3
      case Thirsty =>
        "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry =>
        "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case AskSold =>
        session.getCurrentUser match
          case Some(user) =>
            if !accountSvc.isAccountExisting(user) then
              "Vous n'avez pas de compte !"
            else
              val solde = accountSvc.getAccountBalance(user)
              "Vous avez " + solde + " CHF sur votre compte."
          case None =>
            "Je ne sais pas qui vous êtes ! Veuillez d'abord vous identifier."

      case AskPrice(command) =>
        "Le prix de votre commande est de " + computePrice(command) + " CHF."
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
                val nouveauSolde = accountSvc.purchase(user, price)
                "Voilà donc " + stringify(
                  command
                ).replaceAll("\\s+"," ") + "! Cela coûte " + price + ". Il vous reste " + nouveauSolde + " CHF sur votre compte."
          case None =>
            "Je ne sais pas qui vous êtes ! Veuillez d'abord vous identifier."
      case Login(user) =>
        // deleted to avoid messing with the web session
        // session.setCurrentUser(user)
        // if !accountSvc.isAccountExisting(user) then
        //   accountSvc.addAccount(user, 30.0) // new account start with 30 CHF
        "Bonjour " + user + " !"
      case _ => "Je ne comprends pas votre demande."
end AnalyzerService
