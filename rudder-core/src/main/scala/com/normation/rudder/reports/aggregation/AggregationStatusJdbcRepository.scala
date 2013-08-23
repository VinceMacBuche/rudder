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

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import net.liftweb.common._
import com.normation.rudder.repository.jdbc._
import java.sql.Timestamp
import org.squeryl.KeyedEntity
import org.joda.time.DateTime
import net.liftweb.util.Helpers.tryo

class AggregationStatusJdbcRepository (
    sessionProvider : SquerylConnectionProvider
) extends AggregationStatusRepository with Loggable {
  import AggregationConstants._

  def getAggregationStatus : Box[Option[(Int,DateTime)]] = {
    getValue(AGGREGATION_STATUS)
  }

  private def getValue(key : String) : Box[Option[(Int,DateTime)]] = {
    try {
      sessionProvider.ourTransaction {
        val q = from(AggregationStatusTable.aggregationStatus)(entry =>
	        where(entry.key === key)
	        select(entry)
        )
        val result = q.toList

        result match {
          case Nil => Full(None)
          case head :: Nil => Full(Some((head.lastId,new DateTime(head.date))))
          case _ =>
            val msg = s"Too many entry matching ${key} in table aggregationStatus "
            logger.error(msg)
            Failure(msg)
        }
      }
    } catch {
     case e:Exception => Failure(s"Error while fetching ${key} in table aggregationStatus :$e")
    }

  }

  def setAggregationStatus(newId : Int, reportsDate : DateTime) : Box[UpdateEntry] = {
    setValue(AGGREGATION_STATUS, newId, reportsDate)
  }

  private def setValue(key : String, reportId : Int, reportsDate : DateTime) : Box[UpdateEntry] = {
    try {
      sessionProvider.ourTransaction {
        val timeStamp = toTimeStamp(reportsDate)
      	val q = update(AggregationStatusTable.aggregationStatus)(entry =>
        	where(entry.key === key)
        	set(entry.lastId := reportId, entry.date := timeStamp))
        val entry = new UpdateEntry(key, reportId, timeStamp)
        logger.info(q.toString)
        if (q ==0) // could not update
          Full(AggregationStatusTable.aggregationStatus.insert(entry))
        else {
          Full(entry)
        }
      }
    } catch {
     case e:Exception => Failure(s"Error while setting ${key} in table aggregationStatus cause is: $e")
    }
  }

}

case class UpdateEntry(
    @Column("key")    key    : String,
    @Column("lastid") lastId : Int,
    @Column("date")   date   : Timestamp
) extends KeyedEntity[String]  {
	def id = key
}

object AggregationStatusTable extends Schema {
  val aggregationStatus = table[UpdateEntry]("aggregationstatus")
}