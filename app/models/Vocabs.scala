package models

import slick.driver.H2Driver.api._
import play.api.libs.json._

case class Vocab(userId: String, question: String, answerStr: String, lastTime: Long, coolDownLevel: Int) {
  def answers = Json.parse(answerStr).as[Seq[String]]
}

class Vocabs(tag: Tag) extends Table[Vocab](tag, "VOCABS") {
  def userId = column[String]("USER_ID")
  def question = column[String]("QUESTION")
  def answerStr = column[String]("ANSWERSTR")
  def lastTime = column[Long]("LAST_TIME")
  def coolDownLevel = column[Int]("COOL_DOWN_LEVEL")
  def * = (userId, question, answerStr, lastTime, coolDownLevel) <> (Vocab.tupled, Vocab.unapply)
  def pk = primaryKey("pk_uq", (userId, question))
}
