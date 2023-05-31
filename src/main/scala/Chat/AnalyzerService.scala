package Chat
import Data.{AccountService, ProductService, Session}
import Vector._
import scala.quoted.Expr

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):
  import ExprTree._

// Compute on the left side of the tree
  def computePriceLeft(t: ExprTree): Double =
    t match
      case Command(_, _, _) => computePrice(t)
      case And(left, right) => computePrice(left)
      case Or(left, right)  => computePrice(left)
      case _                => println(s"we ended up erroring 1\n${t}"); Double.NaN
  def computeStringLeft(t: ExprTree): String =
    t match
      case Command(_, _, _) => stringify(t)
      case And(left, right) => stringify(left)
      case Or(left, right)  => stringify(left)
      case _                => println(s"we ended up erroring 2\n${t}"); "ERROR"

  def foldLeft(acc: Double, t: ExprTree): Double =
    t match 
      case Or(left, right: Command) =>
        acc min computePrice(right)
      case And(left, right: Command) =>
        acc + computePrice(right)
      case Or(left, right) =>
        foldLeft(acc min computePriceLeft(right), right)
      case And(left, right) =>
        foldLeft(acc + computePriceLeft(right), right)
      case _ => { println(s"we ended up erroring 3 \n${t}"); Double.NaN }

  /** Compute the price of the current node, then returns it. If the node is not
    * a computational node, the method returns 0.0. For example if we had a "+"
    * node, we would add the values of its two children, then return the result.
    * @return
    *   the result of the computation
    */
  // DONE - Part 2 Step 3
  def computePrice(t: ExprTree): Double =
    t match
      case Command(num, product, brand) =>
        val brandName = brand.getOrElse(productSvc.getDefaultBrand(product));
        val price = productSvc.getPrice(product, brandName)
        num * price
      case Or(left, right) =>
        foldLeft(computePrice(left), t)
      case And(left, right) =>
        foldLeft(computePrice(left), t)
      case _ => { println(s"we ended up erroring 4\n${t}"); Double.NaN }

  def computePrice_left(t: ExprTree) : Double = 
    t match
      case Command(num, product, brand) =>
        val brandName = brand.getOrElse(productSvc.getDefaultBrand(product));
        val price = productSvc.getPrice(product, brandName)
        num * price
      case Or(left, right) =>
        if computePrice_left(left) < computePrice_left(right) then computePrice_left(left)
        else computePrice_left(right)
      case And(left, right) =>
        computePrice_left(left) + computePrice_left(right)
      case _ => { println(s"we ended up erroring 4\n${t}"); Double.NaN }
  def stringify_left(t: ExprTree) : String =
    t match
      case Command(1, product, brand) => s"1 $product ${brand.getOrElse("")}"
      case Command(n, product, brand) =>
        s"$n ${product}s ${brand.getOrElse("")}"
      case And(left, right) =>
        s"$stringify_left(left) et $stringify_left(right)"
      case Or(left, right) =>
        if computePrice_left(left) > computePrice_left(right) then s"${stringify_left(right)}"
        else s"${stringify_left(left)}"
      case _ => { println("i don't know how we ended up here 2"); "ERROR " }
  def simplify_left(t: ExprTree): ExprTree = 
    t match
      case Command(_,_,_) => t
      case Or(left, right) =>
        if computePrice_left(left) < computePrice_left(right) then simplify_left(left)
        else simplify_left(right)
      case And(left, right) => 
        val l = simplify_left(left)
        val r = simplify_left(right)
        And(l,r)
      case _ => { println(s"we ended up erroring with tree\n${t}"); t }
  /**
    * Simplify the tree by removing useless nodes.
    * nous : And(Command(1,biere,None),Or(Command(2,croissant,None),Command(1,biere,Some(punkipa))))
      Eux : Or(
              And(Product(1,biere,None),
                  Product(2,croissant,None))
              ,Product(1,biere,Some(punkipa))))"
    *
    * @param t
    * @return
    */
  def reverseTree(t:ExprTree):ExprTree =
    def notation_polonaise(inner_t:ExprTree, acc_cmd:List[ExprTree],acc_op:List[String]) : ExprTree = 
      inner_t match
        case And(left:Command, right:Command) => notation_polonaise(Hungry,right::left::acc_cmd, "et"::acc_op)
        case And(left, right) => notation_polonaise(right, left::acc_cmd, "et"::acc_op)
        case Or(left:Command, right:Command) => notation_polonaise(Hungry, right::left::acc_cmd, "ou"::acc_op)
        case Or(left, right) => notation_polonaise(right,  left::acc_cmd, "ou"::acc_op)
        case Command(_,_,_) => t
        case _ => {
          print(acc_op,acc_cmd);
          var ops = acc_op.reverse 
          var cmds = acc_cmd.reverse
          val cmd1= cmds.head
          val cmd2= cmds.tail.head
          var acc = ops.head match
            case "et" => And(cmd1,cmd2)
            case "ou" => Or(cmd1,cmd2)
          cmds = cmds.drop(2)
          ops = ops.tail
          while !ops.isEmpty do
            val cmd = acc_cmd.head
            val op = ops.head
            acc = op match
              case "et" => And(acc,cmd)
              case "ou" => Or(acc,cmd)
            ops = ops.tail
          acc
        }

    notation_polonaise(t,List(),List())  
    

    // def loop(inner_t :ExprTree, acc_cmds : Vector[ExprTree.Command], acc_logic : Vector[ExprTree]):(Vector[ExprTree],Vector[ExprTree]) =
    //   inner_t match
    //     case Command => 
      // t match
      //   case Command(_,_,_)  => t
      //   case Or(left, right) =>
      //     val r = simplifyTree(right)
      //     if computePrice(left) < computePrice(r) then left
      //     else r
      //   case And(left, right) => 
      //     val l = simplifyTree(left)
      //     val r = simplifyTree(right)
      //     if computePrice(l) < computePrice(r) then l
      //     else r
      //   case _ => { println(s"we ended up erroring with tree\n${t}"); t }
    
  def foldLeftString(acc_str: String, acc_sum: Double, t: ExprTree): String =
    t match
      case Or(left, right: Command) =>
        if acc_sum < computePrice(right) then s"${acc_str}"
        else s"${stringify(right)}"
      case And(left, right: Command) =>
        s"${acc_str} et ${stringify(right)}"
      case Or(left, right) =>
        val r =
          if acc_sum < computePriceLeft(right) then acc_str
          else computeStringLeft(right)
        foldLeftString(r, acc_sum min computePriceLeft(right), right)
      case And(left, right) =>
        val r = s"$acc_str ${computeStringLeft(right)}"
        foldLeftString(r, acc_sum + computePriceLeft(right), right)
      case _ => { println(s"we ended up erroring 5\n${t}"); "ERROR" }

  /*
   * Compute the string representation of the current node and the next, then returns it.
   */
  def stringify(t: ExprTree): String =
    t match
      case Command(1, product, brand) => s"1 $product ${brand.getOrElse("")}"
      case Command(n, product, brand) =>
        s"$n ${product}s ${brand.getOrElse("")}"
      case And(left, right) =>
        foldLeftString(s"${stringify(left)} ", computePrice(left), t)
      case Or(left, right) =>
        if computePrice(left) > computePrice(right) then s"${stringify(right)}"
        else s"${stringify(left)}"
      case _ => { println("i don't know how we ended up here 2"); "ERROR " }
      
  /**
    * Return the list of items in the Expression Tree
    */
  
  def list_of_item(session:Session)(t:ExprTree):List[(Int,String)] = 
    def flatten(inner_t : ExprTree, acc : List[(Int,String)]):List[(Int,String)] = 
      inner_t match
        case Thirsty => Nil
        case Hungry => Nil
        case AskSold => Nil
        case Buy(command) => list_of_item(session)(command)
        case AskPrice(command) => list_of_item(session)(command)
        case Login(user) => Nil
        case Or(left, right) => flatten(left,acc) ++ flatten(right,acc)
        case And(left, right) => flatten(left,acc) ++ flatten(right,acc)
        //we compute a hash using the product and the brand
        case Command(num, product, brand) => (num,s"$product${brand.getOrElse("")}") :: acc
    flatten(t,Nil)
    

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
        // session.setCurrentUser(user)
        // if !accountSvc.isAccountExisting(user) then
        //   accountSvc.addAccount(user, 30.0) // new account start with 30 CHF
        "Bonjour " + user + " !"
      case _ => "Je ne comprends pas votre demande."
end AnalyzerService
