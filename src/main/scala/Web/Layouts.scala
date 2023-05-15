package Web

import scalatags.Text.all._
import scalatags.Text.tags2
import scalatags.Text.TypedTag
import Web.Decorators.getSession
import Data.SessionService

/** Assembles the method used to layout ScalaTags
  */
object Layouts:

  private def show_messages = 
    val messages = List() // TODO Next step
    if messages.length > 0 then
      // TODO with a for, next step
      div(
        `class` := "msg",
        span(`class` := "author"),
        span(`class` := "msg-content", span(`class` := "mention")),
      )
    else
      div(
        `class` := "msg-wait",
        p("Please wait, the messages are loading !")
      )

  private def head_template =
    head(
      meta(charset := "utf-8"),
      link(rel := "stylesheet", href := "static/css/main.css"),
      script(src := "static/js/main.js")
    )
  private def navbar_template(username: Option[String]) =
    tags2.nav(
      div("Bot-tender", `class` := "nav-brand"),
      if username.isDefined then
        a("logout", href := "/logout", `class` := "nav-item")
      else
        a("login", href := "/login", `class` := "nav-item")
    )

  def index(username: Option[String]) =
    doctype("html")(
      html(
        head_template,
        body(
          navbar_template(username),
          div(
            `class` := "content",
            div(
              id := "boardMessage",
              show_messages
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
    doctype(
      "html"
    )(
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
    doctype(
      "html"
    )(
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
