package controllers

import java.util.UUID
import javax.inject._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import slick.driver.H2Driver.api._
import play.api.libs.ws.WSClient
import play.api.db.slick.DatabaseConfigProvider
import com.typesafe.config.ConfigFactory
import models._
import slick.driver.JdbcProfile
import slick.jdbc.meta.MTable
import scala.util.Random
import play.api.libs.json._
import markatta.futiles.UnliftException
import markatta.futiles.Lifting.Implicits._
import play.api.Logger

@Singleton
class Main @Inject() (ws: WSClient, dbConfigProvider: DatabaseConfigProvider) extends Controller {
  val conf = ConfigFactory.load()
  val oauthId = conf.getString("oauth.id")
  val oauthRedirect = conf.getString("oauth.redirect")
  val oauthSecret = conf.getString("oauth.secret")

  class UserRequestOpt[A](val userOpt: Option[User], request: Request[A]) extends WrappedRequest[A](request)
  class UserRequest[A](val user: User, request: Request[A]) extends WrappedRequest[A](request)

  object Authenticated extends ActionBuilder[UserRequestOpt] with ActionTransformer[Request, UserRequestOpt] {
    def transform[A](request: Request[A]) = Future.successful {
      val userOpt = for {
        id <- request.session.get("id")
        name <- request.session.get("name")
      } yield {
        User(id, name)
      }
      new UserRequestOpt(userOpt, request)
    }
  }

  object Forced extends ActionRefiner[UserRequestOpt, UserRequest] {
    def refine[A](request: UserRequestOpt[A]) = Future.successful {
      request.userOpt match {
        case Some(user) =>
          Right(new UserRequest(user, request))
        case None =>
          val state = UUID.randomUUID.toString
          val redirect = Redirect(
            "https://accounts.google.com/o/oauth2/v2/auth", Map(
              "response_type" -> "code",
              "client_id" -> oauthId,
              "redirect_uri" -> oauthRedirect,
              "scope" -> "profile",
              "state" -> state
            ).mapValues(v => Seq(v))
          ).withSession("original_uri" -> request.uri, "state" -> state)
          Left(redirect)
      }
    }
  }

  def callback(code: String, state: String) = Action.async { request =>
    if (request.session.get("state").fold(false)(_ == state)) {
      val originalUri = request.session("original_uri")
      ws.url("https://www.googleapis.com/oauth2/v4/token")
        .post(Map(
          "code" -> code,
          "client_id" -> oauthId,
          "client_secret" -> oauthSecret,
          "redirect_uri" -> oauthRedirect,
          "grant_type" -> "authorization_code"
        ).mapValues(Seq(_)))
        .flatMap { resp =>
          val accessToken = (resp.json \ "access_token").as[String]
          ws.url("https://www.googleapis.com/userinfo/v2/me")
            .withHeaders("Authorization" -> ("Bearer " + accessToken))
            .get
            .map { resp =>
              val json = resp.json
              val id = (json \ "id").as[String]
              val name = (json \ "name").as[String]
              Redirect(originalUri).withSession("id" -> id, "name" -> name)
            }
        }
    } else {
      Future.successful(Unauthorized)
    }
  }

  def login = (Authenticated andThen Forced) {
    Redirect(routes.Main.index)
  }

  def logout = Action {
    Redirect(routes.Main.index).withNewSession
  }

  //prepare database
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db = dbConfig.db

  val vocabs = TableQuery[Vocabs]

  db.run(MTable.getTables).foreach { tables =>
    val existTables = tables.map(_.name.name).toSet
    if (!existTables.contains(vocabs.baseTableRow.tableName)) {
      db.run(vocabs.schema.create)
    }
  }

  def pickVocab(user: User) = {
    db.run(vocabs.filter(v => v.userId === user.id).result).map { vocabs =>
      if (vocabs.isEmpty) {
        None
      } else {
        val highPriors = vocabs.filter(v => v.lastTime + v.coolDownLevel * 600000L < System.currentTimeMillis)
        if (highPriors.nonEmpty) {
          Some(Random.shuffle(highPriors).head)
        } else {
          Some(Random.shuffle(vocabs).head)
        }
      }
    }
  }

  def index = Authenticated.async { request =>
    val res = for {
      user <- Future(request.userOpt).unlift("user not found")
      vocab <- pickVocab(user).unlift("vocab not found")
    } yield {
      import views.Index._
      Ok(views.Index(
        Some(user),
        None,
        Some(Content(vocab.question, vocab.answers.map(a => Answer(a, false)), true))
      )).as(HTML)
    }
    res.recover {
      case e: UnliftException =>
        Ok(views.Index(request.userOpt, None, None)).as(HTML)
    }
  }

  def validate = (Authenticated andThen Forced).async(parse.urlFormEncoded) { request =>
    //val reqMap = request.body.mapValues(_.filter(_ != ""))
    val question = request.body("question").filter(_ != "").head
    val answers = request.body("answer").filter(_ != "").toSet
    val mask = request.body("mask").head.toBoolean
    Logger.info(s"${request.user.name} answered ${question}")
    //TODO make DB update atomic here
    db.run {
      vocabs.filter(v => v.userId === request.user.id && v.question === question).result.head
    }.flatMap {
      vocab =>
        val ac = vocab.answers.toSet == answers
        for {
          correctUpdates <- if (mask) {
            val newVocab = vocab.copy(
              lastTime = System.currentTimeMillis,
              coolDownLevel = if (ac) vocab.coolDownLevel + 1 else 1
            )
            db.run(vocabs.insertOrUpdate(newVocab))
          } else Future.successful(0)
          pickedVocab <- pickVocab(request.user).unlift("vocab not found")
        } yield {
          import views.Index._
          Ok(views.Index(
            Some(request.user),
            Some(
              if (ac) {
                Message("正解です、次の質問に答えましょう", "green")
              } else {
                Message("答えは正しくありません", "red")
              }
            ),
            Some(Content(
              if (ac) pickedVocab.question else question,
              if (ac) {
                pickedVocab.answers.map(a => Answer(a, false))
              } else {
                vocab.answers.map(a => Answer(a, !answers.contains(a)))
              },
              ac
            ))
          )).as(HTML)
        }
    }
  }

  def addPage = (Authenticated andThen Forced) { request =>
    Ok(views.Add(request.user)).as(HTML)
  }

  def add = (Authenticated andThen Forced).async(parse.urlFormEncoded) { request =>
    val resOpt = for {
      question <- request.body.get("question").flatMap(_.headOption.filter(_ != ""))
      answers <- request.body.get("answer").map(_.filter(_ != ""))
      action <- request.body.get("action").flatMap(_.headOption)
    } yield {
      action match {
        case "save" if answers.nonEmpty =>
          Logger.info(s"${request.user.name} added ${question}")
          db.run {
            vocabs.insertOrUpdate(Vocab(request.user.id, question, Json.stringify(Json.toJson(answers)), 0L, 0))
          }.map(_ => Redirect(routes.Main.addPage))
        case "delete" =>
          db.run {
            vocabs.filter(v => v.userId === request.user.id && v.question === question).delete
          }.map(_ => Redirect(routes.Main.addPage))
        case _ =>
          Future(BadRequest("Invalid request"))
      }
    }
    resOpt.getOrElse(Future(BadRequest("Invalid request")))
  }

  def suggestions(q: String) = (Authenticated andThen Forced).async { request =>
    if (q == "") {
      Future(BadRequest)
    } else {
      db.run(vocabs.filter(_.userId === request.user.id).map(_.question).result)
        .map(_.filter(_.toLowerCase.contains(q.toLowerCase)))
        .map(seq => Ok(Json.toJson(seq)))
    }
  }

  def answers(q: String) = (Authenticated andThen Forced).async { request =>
    if (q == "") {
      Future(BadRequest)
    } else {
      db.run(vocabs.filter(v => v.userId === request.user.id && v.question === q).result.head)
        .map(vocab => Ok(Json.toJson(vocab.answers)))
    }
  }
}
