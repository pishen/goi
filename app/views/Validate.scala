package views

import scalatags.Text.all._
import Helpers._
import models.User

object Validate {
  def apply(userOpt: Option[User]) = {
    page(
      Seq(
        link(href := "/assets/css/validate.css", rel := "stylesheet")
      ),
      Seq(),
      userOpt,
      Seq(
        div(
          cls := "container",
          div(
            cls := "big-text text-center",
            "programmer"
          ),
          form(
            action := "/validate",
            method := "POST",
            Seq(
              input(`type` := "text"),
              input(`type` := "text")
            ),
            input(`type` := "submit", hidden)
          )
        )
      )
    )
  }
}
