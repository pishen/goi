package views

import scalatags.Text.all._
import Helpers._
import models._

object Index {
  case class Message(text: String, color: String)
  case class Answer(text: String, red: Boolean)
  case class Content(question: String, answers: Seq[Answer], mask: Boolean)
  def apply(userOpt: Option[User], messageOpt: Option[Message], contentOpt: Option[Content]) = {
    page(
      Seq(
        link(href := "//cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.css", rel := "stylesheet"),
        link(href := "/assets/css/big-form.css", rel := "stylesheet"),
        link(href := "/assets/css/index.css", rel := "stylesheet")
      ),
      Seq(
        script(src := "//cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.js"),
        script(src := "/assets/js/index.js")
      ),
      userOpt,
      Seq(
        messageOpt.map(msg => div(id := "message", data.text := msg.text, data.color := msg.color)),
        div(
          cls := "container",
          contentOpt match {
            case Some(content) =>
              form(
                action := "/",
                method := "POST",
                div(cls := "big-text text-center", content.question),
                input(`type` := "hidden", name := "question", value := content.question),
                input(`type` := "hidden", name := "mask", value := content.mask),
                content.answers.zipWithIndex.map {
                  case (answer, i) =>
                    input(
                      Some(cls := "red-answer").filter(_ => answer.red),
                      `type` := "text",
                      name := "answer",
                      autocomplete := "off",
                      Some(placeholder := answer.text).filter(_ => !content.mask),
                      Some(autofocus).filter(_ => content.mask && i == 0)
                    )
                },
                input(`type` := "submit", hidden)
              )
            case None =>
              div(cls := "big-text text-center", "ゴイマシーン")
          }
        )
      )
    )
  }
}
