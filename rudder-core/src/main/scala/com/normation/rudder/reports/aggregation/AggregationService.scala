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

import com.normation.rudder.repository.RuleExpectedReportsRepository
import com.normation.rudder.repository.ReportsRepository
import org.joda.time.DateTime
import com.normation.rudder.domain.reports.bean._
import com.normation.rudder.domain.reports.RuleExpectedReports
import com.normation.inventory.domain.NodeId
import scala.collection.mutable.Buffer
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.RuleId
import org.joda.time.Duration
import java.sql.Timestamp
import net.liftweb.common.Loggable
import com.normation.rudder.domain.reports.bean.Reports._
import com.normation.utils.HashcodeCaching
import org.joda.time.Interval
import net.liftweb.common._
import com.normation.rudder.services.reports.ReportingService
import com.normation.rudder.domain.reports.bean.ReportType._
import com.normation.rudder.domain.reports.RuleExpectedReports

import com.normation.rudder.reports.aggregation.AggregationConstants.AGGREGATION_INTERVAL

/**
 * That service contains most of the logic to merge
 * interval of same criticity together.
 */
class AggregationService(
    expectedReportRepository   : RuleExpectedReportsRepository
  , reportsRepository          : ReportsRepository
  , aggregatedReportsRepository: AggregatedReportsRepository
  , updatesEntriesRepository   : AggregationStatusRepository
  , reportDelay                : Int // in hours
  , maxDays                    : Int // in days
) extends Loggable {
  import AggregationConstants._

  val unitAggregator : UnitAggregationService = new UnitAggregationService()

  def newAggregation = {
    updatesEntriesRepository.getAggregationStatus match {
      case Full(Some((lastReportId,lastReportDate))) =>
        val reports = reportsRepository.getReportsfromId(lastReportId, lastReportDate plusDays(maxDays)) match {
          case Full(reports) =>
            val filteredReports = reports.map(_._1).filter{
              case _:ResultErrorReport => true
              case _:ResultRepairedReport => true
              case _:ResultSuccessReport => true
              case _ => false
            }
            logger.warn(filteredReports.size.toString)
            if (!filteredReports.isEmpty) {
            val onlyReports = filteredReports
            onlyReports.groupBy(ReportKey(_)).map{
              case (reportKey, reports) =>
                logger.info(reports.size.toString)
                implicit def startingTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)
                val beginDate = reports.map(_.executionTimestamp).min

                val endDate = reports.map(_.executionTimestamp).max
                val serialInterVal = SerialInterval(reports.map(_.serial).min,reports.map(_.serial).max)
                val interval = new Interval(beginDate,endDate)
                val expectedReports = expectedReportRepository.findExpectedReports(reportKey.ruleId, Some(beginDate), Some(endDate)).getOrElse(Seq())
                val alreadyAggregated = aggregatedReportsRepository.getAggregatedReportsByDate(reportKey.ruleId, beginDate minusSeconds(AGGREGATION_INTERVAL), endDate plusSeconds(AGGREGATION_INTERVAL)).map(_.filter(_.key == reportKey)).getOrElse(Seq()).toSet
                logger.info(alreadyAggregated)
                val finallyAggreg = if (alreadyAggregated.isEmpty) alreadyAggregated + AggregatedReport(reportKey
                      , 0
                      , SuccessReportType
                      , interval
                      , ""
                      , serialInterVal
                      , None )
                      else alreadyAggregated
                      logger.info(finallyAggreg.size.toString)
                val result = unitAggregator.updateAggregatedReports(reports.map(ExecutionReport(_)), expectedReports, ((reports.map(report => AgentExecution(report.executionTimestamp)))++ (finallyAggreg.map(ag =>AgentExecution(ag.interval.getEnd() minusMinutes(5))))).toSet, finallyAggreg.map(report => AggregationReport(report)))

                val resultToSave = result.toSave.map(AggregatedReport(_, reportKey))
                logger.error(resultToSave)
                logger.info(s"saving ${resultToSave.size} for reportkey $reportKey")
                logger.debug(s"${result.IdsToDelete.size}")
                aggregatedReportsRepository.deleteAggregatedReports(result.IdsToDelete.toSeq)
                aggregatedReportsRepository.saveAggregatedReports(resultToSave.toSeq)
            }
            updatesEntriesRepository.setAggregationStatus(reports.map(_._2).max, reports.maxBy(_._2)._1.executionTimestamp)
            }
            else {
              //logger.error("Nothing to do")
            }
          case _ => //logger.error("error with reports")
        }


      case Full(None) => reportsRepository.getReportsWithLowestId match {
        case Full(Some((report,id))) =>

          updatesEntriesRepository.setAggregationStatus(id, report.executionTimestamp)
        case _ =>
      }
      case eb:EmptyBox =>   reportsRepository.getReportsWithLowestId match {
        case Full(Some((report,id))) =>

        case _ =>
      }

    }

  }
}
