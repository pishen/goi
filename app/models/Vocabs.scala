package models

import slick.driver.H2Driver.api._

case class Vocab(userId: String, question: String, answer: String, correct: Int)

class Vocabs(tag: Tag) extends Table[Vocab](tag, "VOCABS") {
  def userId = column[String]("USER_ID")
  def question = column[String]("QUESTION")
  def answer = column[String]("ANSWER")
  def correct = column[Int]("CORRECT")
  def * = (userId, question, answer, correct) <> (Vocab.tupled, Vocab.unapply)
  def idx = index("idx_user_id", userId)
}
