package Chat
import Data.{AccountService, ProductService, Session}
import Vector._
import scala.quoted.Expr
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import castor.Context.Simple.global // To resolve bug
import scalatags.Text.StringFrag
import Chat.Parser
import Chat.UnexpectedTokenException
import Chat.ExprTree
import Chat.ExprTree.*
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import Utils.FutureOps.*
import scala.util.Success
import scala.util.Failure

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
  def list_of_item(t:ExprTree):HashMap[String,Int] = 
    def flatten(inner_t : ExprTree, acc : HashMap[String, Int]):HashMap[String, Int] = 
      inner_t match
        case Buy(command) => list_of_item(command)
        case AskPrice(command) => list_of_item(command)
        case Or(left, right) => flatten(left,acc) ++ flatten(right,acc)
        case And(left, right) => flatten(left,acc) ++ flatten(right,acc)
        //we compute a hash using the product and the brand
        case Command(num, product, brand) => 
          val prodName = s"$product ${brand.getOrElse(productSvc.getDefaultBrand(product))}"
          acc.get(prodName) match
            case Some(value) => acc.updated(prodName, value + num)
            case None => acc.updated(prodName, num)
        case _ => throw new IllegalArgumentException(s"Expected a Command, got $inner_t")
    flatten(t,HashMap())
    
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

  private def incrOrCreate(o : Option[Int]) = o match 
      case None => Some(1)//la première commande réussit, ajoute 1 produit fini
      case Some(v) => Some(v+1)

  def buy(session: Session)(t: ExprTree):Future[String] = 
    val command = t match
      case Buy(command) => command
      case _ => throw new IllegalArgumentException(s"Expected a Buy, got $t")
    
    var finishedProds: TrieMap[String,Int] = TrieMap()
        var lastProdsInBrand = List[Future[Unit]]()
        val commandText = stringify(command)
        val brandsInCommand = list_of_item(command)
        for (brandName, brandProdNb) <- brandsInCommand do
          
          def createProdFuture(prodName: Option[String] = None) : Future[Unit] = Future{
            var debugStr = "Creating product future "
            prodName match
              // if successful, increment the number of finished product
              case Some(prodName) => 
                debugStr += s"$prodName"
                finishedProds.updateWith(prodName)(incrOrCreate)
              case None => 
                debugStr = "failed"
                () 
            // log.debug(debugStr)
          }


          def makeSameProduct(currentNb: Int, totalNb: Int, prodName: String, previousFuture: Future[Unit]) : Future[Unit] =

            val currentProd = randomSchedule(Duration(2, "seconds"), Duration(1, "seconds"), 0.7) transformWith { 
              case Failure(exception) => createProdFuture()
              case Success(value) => createProdFuture(Some(prodName))
            }

            // si pas le dernier
            if currentNb != totalNb then
              previousFuture.transformWith(
                // On continue lorsque le précédent est fini (reussi ou non)
                _ => makeSameProduct(currentNb+1, totalNb, prodName, currentProd)
              )
            else
              currentProd // retourne le dernier future

          val lastProdOfOneBrand = makeSameProduct(1, brandProdNb, brandName, Future.successful(()))

          // On sauve le dernier future dans la liste
          lastProdsInBrand = lastProdOfOneBrand +: lastProdsInBrand

        //-------------------
        
        // Quand tous les produits sont finis, on envoie le message
        Future.sequence(lastProdsInBrand).transformWith{
          case Success(_) => 
            val nbMadeProd = finishedProds.foldLeft(0)(_ + _._2)
            val nbOrderedProd = brandsInCommand.foldLeft(0)(_ + _._2)

            var msg = s"La commande de ${commandText} "
            if nbMadeProd == 0 then
              msg += "ne peut pas être délivrée."
            else
              // partielle
              if nbMadeProd < nbOrderedProd then
                val partCommand = finishedProds.foldLeft("")(
                  (acc, order) => acc + s"${order._2} ${order._1} "
                )

                msg += s"est partiellement prête. Voici : ${partCommand}."
              // complète
              else
                msg += "est prête !"

              // log.debug(finishedProds)

              val totalPrice = finishedProds.foldLeft(0.0)(
                (acc, order) => acc + order._2 * productSvc.getPriceFromString(order._1)
              )
              
              msg += s" Cela coûte ${totalPrice}.-"
              session.getCurrentUser match
                case Some(user) =>
                  if accountSvc.getAccountBalance(user) < totalPrice then
                    msg += " Vous n'avez pas assez d'argent sur votre compte !"
                  else
                    val nouveauSolde = accountSvc.purchase(user, totalPrice)
                    msg += " Merci !"

                case _ => 
                  // log.debug("User not logged in  and passing a command")
                  () // user has to be logged in to send a message

            
            // msgSvc.add("BotTender", StringFrag(msg) , None, Some(expr), Some(replyToId))
            // log.debug(s"Bot reply: $msg")

            // sendLastMessages // nécessaire car dans un future

            Future{(msg)}
          case Failure(exception) => 
            // normalement on ne devrait pas avoir d'erreur mais on la log
            // log.debug(s" error ${exception}")
            Future{("")}
        }

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
        throw new IllegalArgumentException(s"Buy should be called instead of reply")
      case Login(user) =>
        // deleted to avoid messing with the web session
        // session.setCurrentUser(user)
        // if !accountSvc.isAccountExisting(user) then
        //   accountSvc.addAccount(user, 30.0) // new account start with 30 CHF
        "Bonjour " + user + " !"
      case _ => "Je ne comprends pas votre demande."
end AnalyzerService
