/*
*************************************************************************************
* Copyright 2013 Normation SAS
*************************************************************************************
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as
* published by the Free Software Foundation, either version 3 of the
* License, or (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU Affero GPL v3, the copyright holders add the following
* Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU Affero GPL v3
* licence, when you create a Related Module, this Related Module is
* not considered as a part of the work and may be distributed under the
* license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/agpl.html>.
*
*************************************************************************************
*/

package com.normation.rudder.reports.aggregation

import org.joda.time.DateTime
import com.normation.inventory.domain.NodeId
import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import java.sql.Timestamp
import net.liftweb.util.Helpers.tryo
import net.liftweb.common._
import com.normation.rudder.repository.jdbc.SquerylConnectionProvider

trait ExecutionReportsRepository {

  def getExecutionByNode (nodeId : NodeId) : Box[Seq[ReportExecution]]

  def saveExecutions (executions : Seq[ReportExecution]) : Box[Seq[ReportExecution]]

  def closeExecutions (executions : Seq[ReportExecution]) : Box[Seq[ReportExecution]]

}

case class ExecutionReportsSquerylRepository(
    sessionProvider : SquerylConnectionProvider
) extends ExecutionReportsRepository {

  def getExecutionByNode (nodeId : NodeId) : Box[Seq[ReportExecution]] = {
    tryo ( sessionProvider.ourTransaction {
      val q = from(Executions.executions)(entry =>
        where(
          entry.nodeId === nodeId.value
        )
        select(entry)
      )
      (Seq[ReportExecution]() ++ q)

    } ) ?~! s"Error when trying to get report executions for node '${nodeId.value}'"
  }

  def saveExecutions (executions : Seq[ReportExecution]) : Box[Seq[ReportExecution]] =  {
    try {
      val res = sessionProvider.ourTransaction {
        executions.map(execution => Executions.executions.insert(execution) )
      }
      Full(res)
    } catch {
      case e:Exception => Failure("could not create aggregated reports")
    }
  }


  def closeExecutions (executions : Seq[ReportExecution]) : Box[Seq[ReportExecution]] =  {
    try {
      val res = sessionProvider.ourTransaction {
        executions.map(execution => Executions.executions.update( exec =>
         where( exec.id === execution.id)
         set( exec.isEnded := true)
          ) )
      }
      Full(executions)
    } catch {
      case e:Exception => Failure("could not create aggregated reports")
    }
  }
}

object Executions extends Schema {
  val executions = table[ReportExecution]("executions")

  on(executions)(
    t => declare(
      t.id.is(autoIncremented("executionsid"), primaryKey)
  ) )
}

case class ReportExecution (
    @Column("nodeid") nodeId  : String
  , @Column("date")   date    : Timestamp
  , @Column("ended")  isEnded : Boolean
) extends KeyedEntity[Long] {

  @Column("id") val  id : Long = 0L
}