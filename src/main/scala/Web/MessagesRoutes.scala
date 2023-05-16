package Web

import Chat.{AnalyzerService, TokenizerService}
import Data.{MessageService, AccountService, SessionService, Session}
import Data.MessageImpl

import scala.collection.mutable.ListBuffer
import castor.Context.Simple.global // To resolve bug
import scalatags.Text.StringFrag

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
                     sessionSvc: SessionService)(implicit val log: cask.Logger) extends cask.Routes:
    import Decorators.getSession

    val websockets: ListBuffer[cask.WsChannelActor] = ListBuffer() // (ListBuffer is capable to remove an element by value)

    @getSession(sessionSvc) // This decorator fills the `(session: Session)` part of the `index` method.
    @cask.get("/")
    def index()(session: Session) =
        // DONE - Part 3 Step 2: Display the home page (with the message board and the form to send new messages)
        Layouts.index(session.getCurrentUser.isDefined, msgSvc.getLatestMessages(20))
        
        

    // TODO - Part 3 Step 4b: Process the new messages sent as JSON object to `/send`. The JSON looks
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
    def send(msg: String)(session: Session) = 
      if msg.isEmpty then
        ujson.Obj("success" -> false, "err" -> "Message is empty")
      else if session.getCurrentUser.isEmpty then
        ujson.Obj("success" -> false, "err" -> "No user is logged in")
      else
        log.debug(s"Message sent: $msg")
        // add new message to the database
        if msg.startsWith("@") then
          val (username, message) = msg.splitAt(msg.indexOf(" "))
          msgSvc.add(session.getCurrentUser.get, StringFrag(message) , Some(username), None)
        else
          msgSvc.add(session.getCurrentUser.get, StringFrag(msg) , None, None)

        sendLastMessages
        ujson.Obj("success" -> true, "err" -> "")

    // TODO - Part 3 Step 4c: Process and store the new websocket connection made to `/subscribe`
    @cask.websocket("/subscribe")
    def subscribe(): cask.WebsocketResult  = 
      cask.WsHandler { channel =>
        websockets.addOne(channel)
        cask.WsActor {
          case cask.Ws.Close(_, _) => websockets -= channel
        }
      }

    // TODO - Part 3 Step 4d: Delete the message history when a GET is made to `/clearHistory`
    @cask.get("/clearHistory")
    def clearHistory() = 
      msgSvc.deleteHistory()
      Layouts.statusPage("History cleared !")

    // TODO - Part 3 Step 5: Modify the code of step 4b to process the messages sent to the bot (message
    //      starts with `@bot `). This message and its reply from the bot will be added to the message
    //      store together.
    //
    //      The exceptions raised by the `Parser` will be treated as an error (same as in step 4b)
    // TODO Eliott : reply to ID for the bot

    initialize()
end MessagesRoutes
