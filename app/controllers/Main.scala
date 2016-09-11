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
  
  def index = Authenticated.async { request =>
    val showCorrect = request.session.get("correct").isDefined
    val res = request.userOpt match {
      case Some(user) =>
        db.run(vocabs.filter(v => v.userId === user.id).result).map { vocabs =>
          Random.shuffle(vocabs).headOption match {
            case Some(vocab) =>
              val answerSize = vocabs.filter(_.question == vocab.question).size
              views.Index(request.userOpt, showCorrect, Some(vocab.question), answerSize)
            case None =>
              views.Index(request.userOpt, showCorrect, None, 0)
          }
        }
      case None =>
        Future(views.Index(request.userOpt, showCorrect, None, 0))
    }
    res.map {
      Ok(_).as(HTML).withSession(request.session - "correct")
    }
  }
  
  def validate = Authenticated(parse.urlFormEncoded) { request =>
    //TODO
    Redirect(routes.Main.index).withSession(request.session + ("correct" -> "true"))
  }
  
  def addPage = (Authenticated andThen Forced) { request =>
    Ok(views.Add(request.user)).as(HTML)
  }
  
  def add = (Authenticated andThen Forced).async(parse.urlFormEncoded) { request =>
    val resOpt = for {
      question <- request.body.get("question").flatMap(_.headOption)
      answers <- request.body.get("answer").map(_.filter(_ != ""))
      action <- request.body.get("action").flatMap(_.headOption)
    } yield {
      action match {
        case "save" =>
          //TODO validate duplication and remove non exist ones
          val insert = vocabs ++= answers.map { answer =>
            Vocab(request.user.id, question, answer, 0)
          }
          db.run(insert).map(_ => Redirect(routes.Main.index))
        case "delete" =>
          val delete = vocabs.filter(_.question === question).delete
          db.run(delete).map(_ => Redirect(routes.Main.index))
      }
    }
    resOpt.getOrElse(Future(BadRequest("Invalid request")))
  }
}
