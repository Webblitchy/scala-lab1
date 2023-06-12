package Web

import Chat.{AnalyzerService, TokenizerService}
import Data.{MessageService, AccountService, SessionService, Session, ProductService}
import Data.MessageImpl

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



// import Chat.AnalyzerService.list_of_item

/**
  * Assembles the routes dealing with the message board:
  * - One route to display the home page
  * - One route to send the new messages as JSON
  * - One route to subscribe with websocket to new messages
  *
  * @param log
  */
class MessagesRoutes(tokenizerSvc: TokenizerService,
                     analyzerSvc: AnalyzerService,
                     msgSvc: MessageService,
                     accountSvc: AccountService,
                     sessionSvc: SessionService,
                     productSvc: ProductService
                     )(implicit val log: cask.Logger) extends cask.Routes:
    import Decorators.getSession

    val websockets: ListBuffer[cask.WsChannelActor] = ListBuffer() // (ListBuffer is capable to remove an element by value)


    def incrOrCreate(o : Option[Int]) = o match 
      case None => Some(1)//la première commande réussit, ajoute 1 produit fini
      case Some(v) => Some(v+1)

    @getSession(sessionSvc) // This decorator fills the `(session: Session)` part of the `index` method.
    @cask.get("/")
    def index()(session: Session) =
        // DONE - Part 3 Step 2: Display the home page (with the message board and the form to send new messages)
        Layouts.index(session.getCurrentUser, msgSvc.getLatestMessages(20))
        
        

    // DONE - Part 3 Step 4b: Process the new messages sent as JSON object to `/send`. The JSON looks
    //      like this: `{ "msg" : "The content of the message" }`.
    //
    //      A JSON object is returned. If an error occurred, it looks like this:
    //      `{ "success" : false, "err" : "An error message that will be displayed" }`.
    //      Otherwise (no error), it looks like this:
    //      `{ "success" : true, "err" : "" }`
    //
    //      The following are treated as error:
    //      - No user is logged in
    //      - The message is empty
    //
    //      If no error occurred, every other user is notified with the last 20 messages
    //

    private def sendLastMessages : Unit = 
      websockets.foreach(
        ws => ws.send(cask.Ws.Text(
          Layouts.messagesInBox(msgSvc.getLatestMessages(20)).foldLeft("")(_ + _.render) // append all messages
        ))
      )

    @getSession(sessionSvc)
    @cask.postJson("/send")
    def send(msg: String)(session: Session) : ujson.Obj = 
      if msg.isEmpty then
        ujson.Obj("success" -> false, "err" -> "Message is empty")
      else if session.getCurrentUser.isEmpty then
        ujson.Obj("success" -> false, "err" -> "No user is logged in")
      else
        log.debug(s"Message sent: $msg")

        if msg.startsWith("@") then
          val (username, message) = msg.splitAt(msg.indexOf(" "))

          // DONE - Part 3 Step 5: Modify the code of step 4b to process the messages sent to the bot (message
          //      starts with `@bot `). 
          //      This message and its reply from the bot will be added to the message
          //      store together. 
          //      => impossible sinon le formattage du message du bot ne sera pas correct et le champ replyToId ne pourra pas être rempli
          // 
          //      The exceptions raised by the `Parser` will be treated as an error (same as in step 4b)
          if username == "@bot" then
            // /!\ L'authentification Web et le compte (avec le pseudo) sont 2 choses complètement différentes
            // Nous avons donc enlever la gestion des session du bot
            try
              // sending the user message
              val replyToId = msgSvc.add(session.getCurrentUser.get, StringFrag(message) , Some(username), None)

              val tokenized = tokenizerSvc.tokenize(message.toLowerCase())

              val parser = new Parser(tokenized)
              val expr = parser.parsePhrases()

              expr match
                case Buy(command) => 

                  val commandText = analyzerSvc.stringify(command)
                  log.debug(s"We tried to stringify the command : ${commandText}")
                  val result = s"Votre commande est en préparation :${commandText}"
                  msgSvc.add("BotTender", StringFrag(result) , None, Some(expr), Some(replyToId))
                  sendLastMessages
                  log.debug(s"La commande a préparer :${analyzerSvc.simplifyTree(command)}")
                  
                  // var finishedProds: TrieMap[String,Int] = TrieMap()
                  // var lastProdsInBrand = List[Future[Unit]]()

                  // val brandsInCommand = analyzerSvc.list_of_item(command)
                  // for (brandName, brandProdNb) <- brandsInCommand do
                    
                  //   def createProdFuture(prodName: Option[String] = None) : Future[Unit] = Future{
                  //     var debugStr = "Creating product future "
                  //     prodName match
                  //       // if successful, increment the number of finished product
                  //       case Some(prodName) => 
                  //         debugStr += s"$prodName"
                  //         finishedProds.updateWith(prodName)(incrOrCreate)
                  //       case None => 
                  //         debugStr = "failed"
                  //         () 
                  //     log.debug(debugStr)
                  //   }


                  //   def makeSameProduct(currentNb: Int, totalNb: Int, prodName: String, previousFuture: Future[Unit]) : Future[Unit] =

                  //     val currentProd = randomSchedule(Duration(2, "seconds"), Duration(1, "seconds"), 0.7) transformWith { 
                  //       case Failure(exception) => createProdFuture()
                  //       case Success(value) => createProdFuture(Some(prodName))
                  //     }

                  //     // si pas le dernier
                  //     if currentNb != totalNb then
                  //       previousFuture.transformWith(
                  //         // On continue lorsque le précédent est fini (reussi ou non)
                  //         _ => makeSameProduct(currentNb+1, totalNb, prodName, currentProd)
                  //       )
                  //     else
                  //       currentProd // retourne le dernier future

                  //   val lastProdOfOneBrand = makeSameProduct(1, brandProdNb, brandName, Future.successful(()))

                  //   // On sauve le dernier future dans la liste
                  //   lastProdsInBrand = lastProdOfOneBrand +: lastProdsInBrand

                  // //-------------------
                  
                  // // Quand tous les produits sont finis, on envoie le message
                  // Future.sequence(lastProdsInBrand).transformWith{
                  //   case Success(_) => 
                  //     val nbMadeProd = finishedProds.foldLeft(0)(_ + _._2)
                  //     val nbOrderedProd = brandsInCommand.foldLeft(0)(_ + _._2)

                  //     var msg = s"La commande de ${commandText} "
                  //     if nbMadeProd == 0 then
                  //       msg += "ne peut pas être délivrée."
                  //     else
                  //       // partielle
                  //       if nbMadeProd < nbOrderedProd then
                  //         val partCommand = finishedProds.foldLeft("")(
                  //           (acc, order) => acc + s"${order._2} ${order._1} "
                  //         )

                  //         msg += s"est partiellement prête. Voici : ${partCommand}."
                  //       // complète
                  //       else
                  //         msg += "est prête !"

                  //       log.debug(finishedProds)

                  //       val totalPrice = finishedProds.foldLeft(0.0)(
                  //         (acc, order) => acc + order._2 * productSvc.getPriceFromString(order._1)
                  //       )
                        
                  //       msg += s" Cela coûte ${totalPrice}.-"
                  //       session.getCurrentUser match
                  //         case Some(user) =>
                  //           if accountSvc.getAccountBalance(user) < totalPrice then
                  //             msg += " Vous n'avez pas assez d'argent sur votre compte !"
                  //           else
                  //             val nouveauSolde = accountSvc.purchase(user, totalPrice)
                  //             msg += " Merci !"

                  //         case _ => 
                  //           log.debug("User not logged in  and passing a command")
                  //           () // user has to be logged in to send a message

                      
                  //     msgSvc.add("BotTender", StringFrag(msg) , None, Some(expr), Some(replyToId))
                  //     log.debug(s"Bot reply: $msg")

                  //     sendLastMessages // nécessaire car dans un future

                  //     Future{()}
                  //   case Failure(exception) => 
                  //     // normalement on ne devrait pas avoir d'erreur mais on la log
                  //     // log.debug(s" error ${exception}")
                  //     Future{()}
                  // }
                  val f = analyzerSvc.buy(session)(expr)
                  f.transformWith{
                    case Success(msg) => 
                      msgSvc.add("BotTender", StringFrag(msg) , None, Some(expr), Some(replyToId))
                      sendLastMessages // nécessaire car dans un future
                      Future{()}
                    case Failure(exception) =>
                      // normalement on ne devrait pas avoir d'erreur mais on la log
                      log.debug(s" error ${exception}")
                      Future{()}
                  }
                  // msgSvc.add("BotTender", StringFrag(msg) , None, Some(expr), Some(replyToId))
            // log.debug(s"Bot reply: $msg")

                  // sendLastMessages // nécessaire car dans un future
                case _ => 
                  val response = analyzerSvc.reply(session)(expr)// NE MODIFE PAS LA SESSION si c'est un je suis _xy
                  msgSvc.add("BotTender", StringFrag(response) , None, Some(expr), Some(replyToId))
                  log.debug(s"Bot reply: $response")
            catch
              case e: UnexpectedTokenException => 
                return ujson.Obj("success" -> false, "err" -> e.getMessage) // to avoid sending success message
          else
            msgSvc.add(session.getCurrentUser.get, StringFrag(message) , Some(username), None)
        else
          msgSvc.add(session.getCurrentUser.get, StringFrag(msg) , None, None)

        sendLastMessages
        ujson.Obj("success" -> true, "err" -> "")

    // DONE - Part 3 Step 4c: Process and store the new websocket connection made to `/subscribe`
    @cask.websocket("/subscribe")
    def subscribe(): cask.WebsocketResult  = 
      cask.WsHandler { channel =>
        websockets.addOne(channel)
        cask.WsActor {
          case cask.Ws.Close(_, _) => websockets -= channel
        }
      }

    // DONE - Part 3 Step 4d: Delete the message history when a GET is made to `/clearHistory`
    @cask.get("/clearHistory")
    def clearHistory() = 
      msgSvc.deleteHistory()
      Layouts.statusPage("History cleared !")


    initialize()
end MessagesRoutes
