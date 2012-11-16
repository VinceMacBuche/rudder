/*
*************************************************************************************
* Copyright 2012 Normation SAS
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

package com.normation.rudder.batch

import net.liftweb.actor.{LiftActor, LAPinger}
import net.liftweb.common.Loggable
import org.joda.time.DateTime
import com.normation.rudder.services.reports._
import com.normation.rudder.domain.Constants
import com.normation.rudder.domain.reports.bean._
import com.normation.rudder.domain.policies._
import com.normation.inventory.domain.NodeId
import com.normation.rudder.repository._
import com.normation.rudder.services.nodes.NodeInfoService
import com.normation.rudder.domain.logger.AllReportLogger

object StartAutomaticReporting

case class ReportLine (
    ruleId : RuleId
  , nodeId : NodeId
  , status : ReportType
  , directive : DirectiveId
  , component : String
  , value : String  
  
)

/**
 * A class that periodically check if the Technique Library was updated.
 * 
 * updateInterval has a special semantic:
 * - for 0 or a negative value, does not start the updater
 * - for updateInterval between 1 and a minimum value, use the minimum value
 * - else, use the given value. 
 */
class AutomaticReportLogger(
    reportService      : ReportingService
  , reportsRepository  : ReportsRepository
  , ruleRepository     : RuleRepository
  , directiveRepository :DirectiveRepository
  , nodeInfoService    : NodeInfoService
  , updateInterval     : Int
) extends Loggable {
  
  private val propertyName = "rudder.batch.techniqueLibrary.updateInterval"
   
  //start batch
  if(updateInterval < 1) {
    logger.info("Disable dynamic group updates sinces property %s is 0 or negative".format(propertyName))
  } else {
    logger.trace("***** starting Technique Library Update batch *****")
    (new LAAutomaticReportLogger) ! StartAutomaticReporting
  }

  ////////////////////////////////////////////////////////////////
  //////////////////// implementation details ////////////////////
  ////////////////////////////////////////////////////////////////
  
  private class LAAutomaticReportLogger extends LiftActor with Loggable {
    
    private[this] var HighestId   = reportsRepository.getHighestId.getOrElse(0)
    private[this] var Reportskind = Seq(Reports.LOG_REPAIRED,Reports.RESULT_ERROR,Reports.RESULT_REPAIRED) 
    private[this] var realUpdateInterval = {
      if(updateInterval < 5) {
        logger.warn("Value '%s' for %s is too small, using '%s'".format(
           updateInterval, propertyName, 1
        ))
        5
      } else {
        updateInterval
      }
    }
    private val reportLine = " Report sent by node '%s' executed on %s for Rule '%s' is in status '%s' on directive '%s', component '%s', value '%s' with message : '%s'"
    override protected def messageHandler = {
      //
      //Ask for a new dynamic group update
      //
      case StartAutomaticReporting => 
        //schedule next update, in minutes
        LAPinger.schedule(this, StartAutomaticReporting, 1000L*60)      
        logger.trace("***** Start a new update")
        val tmpID = reportsRepository.getHighestId.getOrElse(HighestId)
        val reports = reportsRepository.getReportsAfter(HighestId)
        
        for {
          report <- reports.filter(report => Reportskind.contains(report.severity)).sortWith((r1,r2) => r1.executionDate.isBefore(r2.executionDate))
          val execDate = report.executionDate.toString("yyyy-MM-dd HH:mm:ss")
          val rule = ruleRepository.get(report.ruleId).map(_.name).getOrElse(report.ruleId.toString())
          val severity = report.severity
          val node :String = nodeInfoService.getNodeInfo(report.nodeId).map(_.hostname).getOrElse(report.nodeId.value)
          val directive : String = directiveRepository.getDirective(report.directiveId).map(_.name).getOrElse(report.directiveId.value)
          val component = report.component
          val keyValue =  report.keyValue
          val message = report.message
        }
        AllReportLogger.FindLogger(severity)(reportLine.format(node,execDate,rule,severity,directive,component,keyValue,message))
        
        HighestId = tmpID
      case _ => 
        logger.error("Ignoring start update dynamic group request because one other update still processing".format())
    }
    /*
    def batchestoReportLine = */
  }
}


