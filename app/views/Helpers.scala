package views

import scalatags.Text.all
import scalatags.Text.all._
import scalatags.Text.tags2
import models.User

object Helpers {
  def page(styles: Seq[Modifier], scripts: Seq[Modifier], userOpt: Option[User], content: Seq[Modifier]) = {
    "<!DOCTYPE html>" + all.html(
      head(
        meta(
          name := "viewport",
          all.content := "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
        ),
        tags2.title("ゴイマシーン"),
        link(
          href := "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
          rel := "stylesheet"
        ),
        link(
          href := "/assets/css/helpers.css",
          rel := "stylesheet"
        ),
        styles
      ),
      body(
        userOpt match {
          case Some(user) =>
            div(
              cls := "identity dropdown",
              button(
                cls := "btn btn-default dropdown-toggle",
                `type` := "button",
                id := "dropdownMenu",
                data.toggle := "dropdown",
                aria.haspopup := "true",
                aria.expanded := "true",
                (user.name + " "),
                span(cls := "caret")
              ),
              ul(
                cls := "dropdown-menu dropdown-menu-right",
                aria.labelledby := "dropdownMenu",
                li(a(href := "/", "テスト")),
                li(a(href := "/add", "語彙を追加/編集")),
                li(a(href := "/logout", "ログアウト"))
              )
            )
          case None =>
            a(cls := "identity btn btn-default", href := "/login", role := "button", "ログイン")
        },
        content,
        footer(),
        script(src := "https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"),
        script(src := "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"),
        scripts
      )
    )
  }
}
