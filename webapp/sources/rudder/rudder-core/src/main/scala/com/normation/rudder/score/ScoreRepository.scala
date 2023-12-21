/*
 *************************************************************************************
 * Copyright 2022 Normation SAS
 *************************************************************************************
 *
 * This file is part of Rudder.
 *
 * Rudder is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In accordance with the terms of section 7 (7. Additional Terms.) of
 * the GNU General Public License version 3, the copyright holders add
 * the following Additional permissions:
 * Notwithstanding to the terms of section 5 (5. Conveying Modified Source
 * Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU General
 * Public License version 3, when you create a Related Module, this
 * Related Module is not considered as a part of the work and may be
 * distributed under the license agreement of your choice.
 * A "Related Module" means a set of sources files including their
 * documentation that, without modification of the Source Code, enables
 * supplementary functions or services in addition to those offered by
 * the Software.
 *
 * Rudder is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Rudder.  If not, see <http://www.gnu.org/licenses/>.

 *
 *************************************************************************************
 */

package com.normation.rudder.score

import better.files.File
import com.normation.errors.Inconsistency
import com.normation.errors.IOResult
import com.normation.errors.Unexpected
import com.normation.inventory.domain.NodeId
import zio._
import zio.syntax._
import com.normation.rudder.db.Doobie
import doobie.Fragments
import doobie.Meta
import doobie.Read
import doobie.Write
import doobie.implicits._
import doobie.implicits.javasql._
import doobie.implicits.toSqlInterpolator

trait ScoreRepository {
  def getAll(): IOResult[Map[NodeId,GlobalScore]]
  def get(id:            NodeId): IOResult[GlobalScore]
  def delete(id:         NodeId): IOResult[NodeId]
  def save(nodeId: NodeId, globalScore: GlobalScore): IOResult[GlobalScore]
}

object ScoreRepositoryImpl {

  import ScoreSerializer._
  import Doobie.DateTimeMeta
  import com.normation.rudder.db.json.implicits._
  import doobie._

  implicit val getScoreValue : Get[ScoreValue] = Get[String].temap(ScoreValue.fromString)

  //implicit val stateWrite: Meta[Score[_]] = new Meta(pgDecoderGet, pgEncoderPut)

  implicit val eventWrite: Write[(NodeId,GlobalScore)] = {
    Write[(String, String, String, List[NoDetailsScore])].contramap {
      case (nodeId: NodeId, score: GlobalScore) =>
        (nodeId.value, score.value.value, score.message, score.details)
    }
  }

  implicit val eventRead: Read[(NodeId,GlobalScore)] = {
    Read[(String, ScoreValue, String, List[NoDetailsScore])].map {
      d: (String, ScoreValue, String, List[NoDetailsScore]) =>
        (NodeId(d._1), GlobalScore(
          d._2,
          d._3,
          d._4
        ))
    }
  }


  def make(
      //campaignSerializer:      ScoreSerializer,
      //path:                    File,
      //campaignEventRepository: ScoreEventRepository
  ): IOResult[ScoreRepositoryImpl] = {
    IOResult.attempt {
      if (path.exists) {
        if (!path.isDirectory || !path.isWritable) {
          Unexpected(s"Score configuration repository is not a writable directory: " + path.pathAsString).fail
        } else ZIO.unit
      } else {
        path.createDirectoryIfNotExists(createParents = true).succeed
      }
    } *>
    new ScoreRepositoryImpl(campaignSerializer, path, campaignEventRepository).succeed
  }
}

class ScoreRepositoryImpl(campaignSerializer: ScoreSerializer, path: File, campaignEventRepository: ScoreEventRepository)
    extends ScoreRepository {

  def getAll(typeFilter: List[ScoreType], statusFilter: List[ScoreStatusValue]): IOResult[List[Score]] = {
    for {
      jsonFiles          <- IOResult.attempt(path.collectChildren(_.extension.exists(_ == ".json")))
      campaigns          <- (ZIO.foreach(jsonFiles.toList) { json =>
                              (for {
                                c <-
                                  campaignSerializer.parse(json.contentAsString)
                              } yield {
                                c
                              }).either.chainError("Error when getting all campaigns from filesystem")
                            })
      (errs, campaignRes) = campaigns.partitionMap(identity)
      _                  <- ZIO.foreach(errs)(err => ScoreLogger.error(err.msg))
      filteredScore    = Score.filter(campaignRes, typeFilter, statusFilter)
    } yield {
      filteredScore
    }
  }
  def get(id: ScoreId):                                                             IOResult[Score]       = {
    for {
      content  <- IOResult.attempt(s"error when getting campaign file for campaign with id '${id.value}'") {
                    val file = path / (s"${id.value}.json")
                    file.createFileIfNotExists(createParents = true)
                    file
                  }
      campaign <- campaignSerializer.parse(content.contentAsString)
    } yield {
      campaign
    }
  }
  def save(nodeId: NodeId, globalScore: GlobalScore): IOResult[(NodeId,GlobalScore)]       = {
    import doobie._
    val query = {
      sql"""insert into Score (nodeId,score, message, details) values (${(nodeId, globalScore)})
           |  ON CONFLICT (nodeId) DO UPDATE
           |  SET score = ${globalScore.value}, message = ${globalScore.message}, details = ${globalScore.details} ; """.stripMargin
    }

    transactIOResult(s"error when inserting event with id ${c.id.value}")(xa => query.update.run.transact(xa)).map(_ => c)

  }

  def delete(id: ScoreId): IOResult[ScoreId] = {
    for {
      campaign_deleted <- IOResult.attempt(s"error when delete campaign file for campaign with id '${id.value}'") {
                            val file = path / (s"${id.value}.json")
                            file.delete()
                          }
      events_deleted   <- campaignEventRepository.deleteEvent(campaignId = Some(id))
    } yield {
      id
    }
  }

}
