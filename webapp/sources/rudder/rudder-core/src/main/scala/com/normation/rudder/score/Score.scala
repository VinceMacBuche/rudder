package com.normation.rudder.score

import com.normation.errors.IOResult
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.reports.CompliancePercent
import com.normation.rudder.score.ScoreValue.A
import com.normation.rudder.score.ScoreValue.B
import com.normation.rudder.score.ScoreValue.C
import com.normation.rudder.score.ScoreValue.D
import com.normation.zio._
import zio.Ref
import zio.ZIO
import zio.syntax._

sealed trait ScoreValue

object ScoreValue {
  case object A extends ScoreValue
  case object B extends ScoreValue
  case object C extends ScoreValue
  case object D extends ScoreValue
  case object E extends ScoreValue
}

trait ScoreDetails

case class ComplianceScoreDetails(compliance: CompliancePercent) extends ScoreDetails

trait Score {
  def name:    String
  def value:   ScoreValue
  def message: String
  def details: ScoreDetails
}

case class ComplianceScore(value: ScoreValue, message: String, details: ComplianceScoreDetails) extends Score {
  override def name: String = "compliance"
}

case class GlobalScore(value: ScoreValue, message: String, details: List[Score])

object GlobalScoreService {
  def computeGlobalScore(oldScore: List[Score], scores: List[Score]): GlobalScore = {

    val correctScores = scores.foldRight(oldScore) {
      case (newScore, acc) =>
        newScore :: acc.filterNot(_.name == newScore.name)
    }
    import ScoreValue._
    val score         = if (correctScores.exists(_.value == E)) { E }
    else if (correctScores.exists(_.value == D)) { D }
    else if (correctScores.exists(_.value == C)) {
      C
    } else if (correctScores.exists(_.value == B)) {
      B
    } else A
    GlobalScore(score, "", correctScores)
  }
}

trait ScoreEvent

case class ComplianceScoreEvent(nodeId: NodeId, compliancePercent: CompliancePercent) extends ScoreEvent

trait ScoreEventHandler {
  def handle(event: ScoreEvent): IOResult[List[(NodeId, List[Score])]]
}

object ComplianceScoreEventHandler extends ScoreEventHandler {
  def handle(event: ScoreEvent): IOResult[List[(NodeId, List[Score])]] = {
    event match {
      case ComplianceScoreEvent(n, p) =>
        val score = if (p.compliance >= 100) {
          ComplianceScore(A, "Node is compliant at 100%", ComplianceScoreDetails(p))
        } else if (p.compliance >= 75) {
          ComplianceScore(B, "Node is compliant at least at 75%", ComplianceScoreDetails(p))
        } else if (p.compliance >= 50) {
          ComplianceScore(C, "Node is compliant at least at 50%", ComplianceScoreDetails(p))
        } else if (p.compliance >= 25) {
          ComplianceScore(D, "Node is compliant at least at 25%", ComplianceScoreDetails(p))
        } else {
          ComplianceScore(D, "Node is compliant at less then 25%", ComplianceScoreDetails(p))
        }

        ((n, score :: Nil) :: Nil).succeed
      case _                          => Nil.succeed
    }
  }
}

class ScoreService {
  val cache: Ref[Map[NodeId, GlobalScore]] = Ref.make(Map[NodeId, GlobalScore]()).runNow

  val handlers: Ref[List[ScoreEventHandler]] = Ref.make(ComplianceScoreEventHandler :: List.empty[ScoreEventHandler]).runNow

  def registerHandler(handler: ScoreEventHandler) = {
    handlers.update(handler :: _)
  }

  def cleanScore(name: String)            = {
    for {
      _ <- cache.update(_.map { case (id, gscore) => (id, gscore.copy(details = gscore.details.filterNot(_.name == name))) })
    } yield {}
  }
  def handleEvent(scoreEvent: ScoreEvent) = {
    for {
      h            <- handlers.get
      newScore     <- ZIO.foreach(h)(_.handle(scoreEvent)).map(_.flatten.groupMapReduce(_._1)(_._2)(_ ++ _))
      c            <- cache.get
      updatedValue  = (for {
                        (nodeId, newScores) <- newScore
                      } yield {
                        val oldScores = c.get(nodeId) match {
                          case None           => Nil
                          case Some(oldScore) => oldScore.details
                        }
                        (nodeId, GlobalScoreService.computeGlobalScore(oldScores, newScores))
                      })
      updatedCache <- ZIO.foreach(updatedValue.toList) { case (nodeId, score) => cache.update(_.+((nodeId, score))) }

    } yield {}
  }

}
