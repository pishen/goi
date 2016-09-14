package views

import scalatags.Text.all._
import Helpers._
import models._

object Add {
  def apply(user: User) = {
    page(
      Seq(
        link(href := "//cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.css", rel := "stylesheet"),
        link(href := "/assets/css/awesomplete.css", rel := "stylesheet"),
        link(href := "/assets/css/big-form.css", rel := "stylesheet"),
        link(href := "/assets/css/add.css", rel := "stylesheet")
      ),
      Seq(
        script(src := "//cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.js"),
        script(src := "/assets/js/awesomplete.min.js"),
        script(src := "/assets/js/add.js")
      ),
      Some(user),
      Seq(
        div(
          cls := "container",
          form(
            cls := "vocabs",
            action := "/add",
            method := "POST",
            input(cls := "question", placeholder := "問", `type` := "text", name := "question", autocomplete := "off", autofocus, required),
            input(cls := "answer", placeholder := "答", `type` := "text", name := "answer", autocomplete := "off"),
            div(
              cls := "actions text-center",
              input(cls := "action", `type` := "hidden", name := "action", value := "save"),
              button(cls := "text-success", `type` := "submit", "保存"),
              button(cls := "text-danger delete-btn", `type` := "button", "削除")
            )
          )
        )
      )
    )
  }
}
