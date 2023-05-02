package Web

import scalatags.Text.all._
import scalatags.Text.tags2

/** Assembles the method used to layout ScalaTags
  */
object Layouts:

  def index =
    doctype("html")(
      html(
        link(rel := "stylesheet", href := "static/css/main.css"),
        script(src := "static/js/main.js"),
        body(
          tags2.nav(
            div("Bot-tender", `class` := "nav-brand"),
            a("login",href:="/login",`class` := "nav-item")
          ),
          div(
            `class` := "content",
            div(
              id := "boardMessage",
              div(
                `class` := "msg",
                span(`class` := "author"),
                span(`class` := "msg-content", span(`class` := "mention"))
              )
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
end Layouts
