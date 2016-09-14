package views

import scalatags.Text.all._
import Helpers._
import models._

object Index {
  def apply(userOpt: Option[User], showCorrect: Boolean, question: Option[String], answerSize: Int) = {
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
        div(id := "last-question", data.correct := showCorrect.toString),
        div(
          cls := "container",
          question match {
            case Some(q) =>
              form(
                action := "/validate",
                method := "POST",
                div(cls := "big-text text-center", q),
                input(`type` := "hidden", name := "question", value := q),
                input(`type` := "text", name := "answer", autofocus),
                Seq.fill(answerSize - 1)(input(`type` := "text", name := "answer")),
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
