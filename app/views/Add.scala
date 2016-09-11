package views

import scalatags.Text.all._
import Helpers._
import models.User

object Add {
  def apply(user: User) = {
    page(
      Seq(
        link(href := "/assets/css/big-form.css", rel := "stylesheet"),
        link(href := "/assets/css/add.css", rel := "stylesheet")
      ),
      Seq(
        script(src := "/assets/js/add.js")
      ),
      Some(user),
      Seq(
        div(
          cls := "container",
          form(
            action := "/add",
            method := "POST",
            input(placeholder := "質問", `type` := "text", name := "question"),
            input(cls := "answer", placeholder := "答え", `type` := "text", name := "answer", autocomplete := "off"),
            div(
              cls := "actions text-center",
              button(cls := "text-success", `type` := "submit", name := "action", value := "save", "保存"),
              button(cls := "text-danger", `type` := "submit", name := "action", value := "delete", "削除")
            )
          )
        )
      )
    )
  }
}
