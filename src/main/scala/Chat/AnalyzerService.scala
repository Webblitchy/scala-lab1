package Chat
import Data.{AccountService, ProductService, Session}

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
        session.setCurrentUser(user)
        if !accountSvc.isAccountExisting(user) then
          accountSvc.addAccount(user, 30.0) // new account start with 30 CHF
        "Bonjour " + user + " !"
      case _ => "Je ne comprends pas votre demande."
end AnalyzerService
