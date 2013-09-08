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
  , executionRepository        : ReportsExecutionRepository
  , reportDelay                : Int // in hours
  , maxDays                    : Int // in days
) extends Loggable {
  import AggregationConstants._

  val unitAggregator : UnitAggregationService = new UnitAggregationService()

  def newAggregation = {

    // Get aggregation status
    updatesEntriesRepository.getAggregationStatus match {
      // Find it, start aggregation
      case Full(Some((lastReportId,lastReportDate))) =>

        // Get reports of the last id and before last report date plus maxDays
        reportsRepository.getReportsfromId(lastReportId, lastReportDate plusDays(maxDays)) match {
          case Full(reports) =>

            // Filter reports to get only interesting reports
            // We only look for reports with a result error/repaired/success
            // Other types are simply ignored for now
            val filteredReports = reports.map(_._1).filter{
              case _:ResultErrorReport => true
              case _:ResultRepairedReport => true
              case _:ResultSuccessReport => true
              case _ => false
            }

            // Check if there is no interesting reports
            if (!filteredReports.isEmpty) {
              // Save new executions
              executionRepository.saveExecutions(filteredReports.map(report => ReportExecution(report.nodeId,report.executionTimestamp,false)).distinct) match {
                case Full(result) =>
                  logger.debug(s"Saved ${result.size} executions")
                  // Start aggregation
                  aggregate(filteredReports)

                  // Update aggregation status
                  updatesEntriesRepository.setAggregationStatus(reports.map(_._2).max, reports.maxBy(_._2)._1.executionTimestamp)

                case eb:EmptyBox => val fail = eb ?~! "could not save reports executions"
                  logger.error(s"could not save reports execution cause is : ${fail.messageChain}")
              }

            } else {
              logger.debug("No reports to aggregate ")
            }

          case eb:EmptyBox =>
            val fail = eb ?~! "could not get Reports"
            logger.error(s"could not get reports cause is: ${fail.messageChain}")
        }

      // Aggregation status not initialized ... initialize it!
      case Full(None) =>
        reportsRepository.getReportsWithLowestId match {
          case Full(Some((report,id))) =>
            updatesEntriesRepository.setAggregationStatus(id, report.executionTimestamp)
          case Full(None) =>
            logger.debug("no reports in database, could not start aggregation for now")
          case eb:EmptyBox =>
            val fail = eb ?~! "could not get Reports with lowest id"
            logger.error(s"could not get update aggregation status cause is: ${fail.messageChain}")
        }
      case eb:EmptyBox =>
        val fail = eb ?~! "could not get new reports"
        logger.error(s"could not find reports to aggregate cause is: ${fail.messageChain}")


    }


    def aggregate (reports : Seq[Reports])= {

      // Regroup reports by report key
      reports.groupBy(ReportKey(_)).map{
        case (reportKey, reports) =>
          implicit def startingTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)
          //Oldest Report
          val beginDate = reports.map(_.executionTimestamp).min

          // Latest report
          val endDate = reports.map(_.executionTimestamp).max

          val reportInterval = new Interval(beginDate,endDate)

          val newReportsExecutions =  reports.map(report => AgentExecution(report.executionTimestamp)).toSet

          // Serial interval
          val serialInterVal = SerialInterval(reports.map(_.serial).min,reports.map(_.serial).max)

          // Get expected Reports, get an empty sequence if there is no expected Report
          val expectedReports = expectedReportRepository.findExpectedReports(reportKey.ruleId, Some(beginDate), Some(endDate)).getOrElse(Seq())

          // Get already aggregated reports, We need to extends the date interval
          // by aggregation interval (1,5 report interval), to get out of bound reports
          val alreadyAggregated = aggregatedReportsRepository.getAggregatedReportsByReportKey(reportKey, beginDate minusSeconds(AGGREGATION_INTERVAL), endDate.plusSeconds(AGGREGATION_INTERVAL)).getOrElse(Seq()).toSet
          logger.info(alreadyAggregated)

          // check if aggregated are empty if so : create an empty Aggreted Report over the whole interval
          // Since some corrections in the agrgeation this may not be necessary
         /* val finallyAggreg = if (alreadyAggregated.isEmpty)
              Set( AggregatedReport(
                       reportKey
                     , 0
                     , SuccessReportType
                     , reportInterval
                     , ""
                     , serialInterVal
                     , None
               ) )
             else
               alreadyAggregated
            */
           //TODO: To handle missing reports with huge date difference (HUGE  -> over AGGREGATION INTERVAL)
           // Look for report nearer the bounds, The main goal is to create no Answer blocks, if missing they won't be created
           // Before :
           // [ OLD AGGREGATED ]___________________[ AFTER AGGREGATED ]
           // With reports :
           // [ OLD AGGREGATED ]_______RRRRR_______[ AFTER AGGREGATED ]
           // After  :
           // [ OLD AGGREGATED ][NOANS][NEW][NOANS][ AFTER AGGREGATED ]

           // TODO: If aggregated Reports bounds are out of the reports executions bounds
           // add the bounds to the executions ( startTime for before report, endTime for after report)
           val beforeReport = alreadyAggregated.find(_.interval.getStart isBefore beginDate).map(agg => AgentExecution(agg.interval.getStart()))
           //val beforeReport = finallyAggreg.find(_.interval.getStart isBefore beginDate).map(agg => AgentExecution(agg.interval.getStart()))
           val executions = newReportsExecutions ++ beforeReport
           // Aggregation !
           val result = unitAggregator.updateAggregatedReports(
                            reports.map(ExecutionReport(_))
                          , expectedReports
                          , executions
                          , alreadyAggregated.map(report => AggregationReport(report)
                        ) )


           val resultToSave = result.toSave.map(AggregatedReport(_, reportKey))
           logger.debug(resultToSave)
           logger.debug(s"saving ${resultToSave.size} for reportkey $reportKey")
           logger.debug(s"${result.IdsToDelete.size}")
           //Save new results
           aggregatedReportsRepository.deleteAggregatedReports(result.IdsToDelete.toSeq)
           // Delete the others
           aggregatedReportsRepository.saveAggregatedReports(resultToSave.toSeq)
      }
    }
  }
}
