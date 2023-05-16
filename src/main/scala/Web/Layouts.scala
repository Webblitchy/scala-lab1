package Web

import scalatags.Text.all._
import scalatags.Text.tags2
import scalatags.Text.TypedTag
import Web.Decorators.getSession
import Data.SessionService
import Data.MessageService.MsgContent
import Data.MessageService.Username
import Data.Message

/** Assembles the method used to layout ScalaTags
  */
object Layouts:

  def messagesInBox(messages: List[Message]) = 
    if messages.length > 0 then
      messages.map(msg => div(
        `class` := "msg",
        span(
          msg.sender,
          `class` := "author"
        ),
        span(
          span(
            msg.mention.getOrElse(""),
            `class` := "mention"
          ),
          `class` := "msg-content", 
          msg.msg,
        ),
      ))
    else
      List(div(
        `class` := "msg-wait",
        p("Please wait, the messages are loading !")
      ))

  private def head_template =
    head(
      meta(charset := "utf-8"),
      link(rel := "stylesheet", href := "static/css/main.css"),
      script(src := "static/js/main.js")
    )
  private def navbar_template(isConnected: Boolean) =
    tags2.nav(
      div("Bot-tender", `class` := "nav-brand"),
      if isConnected then
        a("logout", href := "/logout", `class` := "nav-item")
      else
        a("login", href := "/login", `class` := "nav-item")
    )

  def index(isConnected: Boolean = false, messages: List[Message] = List()) =
    doctype("html")(
      html(
        head_template,
        body(
          navbar_template(isConnected),
          div(
            `class` := "content",
            div(
              id := "boardMessage",
              messagesInBox(messages),
            ),
            form(
              id := "msgForm",
              attr("onsubmit") := "submitMessageForm();return false",
              div(id := "errorDiv", `class` := "errorMsg"),
              label("Your message:", attr("for") := "messageInput"),
              input(
                attr("type") := "text",
                placeholder := "Write your message",
                id := "messageInput"
              ),
              input(attr("type") := "submit")
            )
          )
        )
      )
    )
  end index

  def login(login_failed : Boolean = false, register_failed : Boolean = false) =
    doctype("html")(
      html(
        head_template,
        body(
          tags2.nav(
            div("Bot-tender", `class` := "nav-brand"),
            a("Go to the message board", href := "/", `class` := "nav-item")
          ),
          div(
            `class` := "content",
            div(
              id := "login_div",
              h1(b("Login")),
              if login_failed then
                div(
                  `class` := "errorMsg",
                  "This username is not valid 🥲"
                )
              else
                div(),
              form(
                action := "/login",
                method := "post",
                id := "login_form",
                label(`for` := "username_login", "Username: "),
                input(id := "username_login",name:="username", `type` := "text"),
                input(`type` := "submit", value := "Login")
              )
            ),
            div(
              id := "register_div",
              h1(b("Register")),
               if register_failed then
                div(
                  `class` := "errorMsg",
                  "This username is already taken 🥲"
                )
              else
                div(),
              form(
                action := "/register",
                method := "post",
                id := "register_form",
                label(`for` := "username_register", "Username: "),
                input(id := "username_register",name:="username", `type` := "text"),
                input(`type` := "submit", value := "Register")
              )
            )
          )
        )
      )
    )

  end login

  def statusPage(message: String) =
    doctype("html")(
      html(
      head_template,
        div(
          `class` := "statusPage",
          div(
            `class` := "statusMsg",
            message
          ),
          a("Go to homepage", href:="/")
        )
      )
    )

end Layouts
