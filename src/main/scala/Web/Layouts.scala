package Web

import scalatags.Text.all._
import scalatags.Text.tags2
import scalatags.Text.TypedTag

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
      link(rel := "stylesheet", href := "static/css/main.css"),
      script(src := "static/js/main.js")
    )
  private def navbar_template =
    tags2.nav(
      div("Bot-tender", `class` := "nav-brand"),
      a("login", href := "/login", `class` := "nav-item")
    )
  def index =
    doctype("html")(
      html(
        head_template(),
        body(
          navbar_template(),
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

  def login =
    doctype(
      "html"
    )(
      html(
        head_template(),
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
              form(
                action := "/login",
                method := "post",
                id := "login_form",
                label(`for` := "username_login", "Username: "),
                input(id := "username_login", `type` := "text"),
                input(`type` := "submit", value := "Login")
              )
            ),
            div(
              id := "register_div",
              h1(b("Register")),
              form(
                action := "/register",
                method := "post",
                id := "register_form",
                label(`for` := "username_register", "Username: "),
                input(id := "username_register", `type` := "text"),
                input(`type` := "submit", value := "Register")
              )
            )
          )
        )
      )
    )

  end login
end Layouts
