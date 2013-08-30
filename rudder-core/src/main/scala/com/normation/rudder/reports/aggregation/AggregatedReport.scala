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

import com.normation.rudder.domain.reports.bean.ReportType
import com.normation.rudder.domain.reports.bean._
import com.normation.rudder.domain.reports.bean.Reports._
import AggregationConstants._
import org.joda.time.DateTime
import org.squeryl.customtypes.TimestampField
import org.joda.time.Interval
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.RuleId
import net.liftweb.common.Loggable
import com.normation.utils.HashcodeCaching


/**
 * AggregatedReport are the "normalized" description of
 * what happened for a given node, for the value of a 
 * component of a directive of a rule. 
 * 
 * Hypothesys: for a given time, for a given AggregatedReportKey,
 * we have AT MOST one aggregatedReport (else, that means that
 * we missed an aggregation somehow). 
 * 
 */



/**
 * This is the report key (that's the most precise
 * targeting of a report).
 *  
 * Notice that its the same key for an aggregated report or
 * a run report. 
 * 
 * Notice that the "id" is not in the key, because the idea
 * is a database thing, not a semantic one. 
 */
case class ReportKey(
    nodeId      : NodeId
  , ruleId      : RuleId
  , directiveId : DirectiveId
  , component   : String
  , keyValue    : String
) extends HashcodeCaching

object ReportKey {
  def apply(entry : Reports) : ReportKey =
    ReportKey(entry.nodeId, entry.ruleId, entry.directiveId, entry.component, entry.keyValue)
}

//a case class for serials
case class SerialInterval(beginingSerial: Int, endingSerial: Int)

object SerialInterval {
  def apply(serial: Int) = new SerialInterval(serial, serial)
}


case class AggregatedReport (
    key         : ReportKey
  , received    : Int   
  , status      : ReportType
  , interval    : Interval
  , message     : String
  , serials     : SerialInterval
  , storageId   : Option[Long]
) extends Loggable {

  if(storageId == Some(0L)) 
    throw new IllegalArgumentException("Storage ID can't be some (0)")
  
  def toSquerylEntity : AggregatedSquerylReport = {
    AggregatedSquerylReport(
        key.nodeId.value
      , key.ruleId.value
      , serials.beginingSerial
      , serials.endingSerial
      , key.directiveId.value
      , key.component
      , key.keyValue
      , status.severity
      , message
      , toTimeStamp(interval.getStart)
      , toTimeStamp(interval.getEnd)
      , received
      , storageId.getOrElse(0L)
    )
  }
}

object AggregatedReport {

  def apply(report: Reports, reportType: ReportType, received: Int) : AggregatedReport = {
    AggregatedReport(
        ReportKey(
            report.nodeId
          , report.ruleId
          , report.directiveId
          , report.component
          , report.keyValue
        )
      , received
      , reportType
      , new Interval(
            report.executionTimestamp
          , report.executionTimestamp
        )
      , report.message
      , SerialInterval(report.serial, report.serial)
      , None
    )
  }

  def apply(report : AggregatedSquerylReport) : AggregatedReport = {
    AggregatedReport(
        ReportKey(
            NodeId(report.nodeId)
          , RuleId(report.ruleId)
          , DirectiveId(report.directiveId)
          , report.component
          , report.keyValue
        )
      , report.received
      , ReportType(report.state)
      , new Interval(
            new DateTime(report.startTime)
          , new DateTime(report.endTime)
        )
      , report.message
      , SerialInterval(report.beginSerial, report.endSerial)
      , if(report.id == 0) None else Some(report.id)
    )
  }
}