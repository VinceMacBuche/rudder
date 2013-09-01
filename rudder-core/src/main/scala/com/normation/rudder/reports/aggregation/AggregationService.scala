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
  , splitMerge                 : SplitMergeAggregatedReport
  , initializeAggregatedReport : InitializeAggregatedReport
  , reportDelay                : Int // in hours
  , maxDays                    : Int // in days
) extends Loggable {
  import AggregationConstants._


//  def checkReport(report: Reports, expectedReport : RuleExpectedReports) = {
//    if ( report.)

//  }




  def newAggregationfromReports (reports:Seq[Reports]) : (Seq[AggregatedReport],Seq[AggregatedReport])= {

    val res : Seq[(Seq[AggregatedReport],Seq[AggregatedReport])] =  reports.groupBy(_.ruleId).flatMap {
      case (ruleId, reportsByRule) =>
        val  executionTimeStamps = reportsByRule.map(_.executionTimestamp).distinct
        val firstExecutionTimeStamp = executionTimeStamps.minBy(_.getMillis())
        val lastExecutionTimeStamp = executionTimeStamps.maxBy(_.getMillis())
        val res : Seq[(Seq[AggregatedReport],Seq[AggregatedReport])] =  expectedReportRepository.findExpectedReports(ruleId, Some(firstExecutionTimeStamp), Some(lastExecutionTimeStamp)) match {
          case eb:EmptyBox => //logger.error("couldn't not fetch expected Reports")
          Seq()
          case Full(expectedReports) =>
            val linearisedExpectedReports = expectedReports.flatMap(splitMerge.lineariseExpectedReport(_)).distinct
            //linearisedExpectedReports.foreach(//logger.warn(_))
            val expectedMap = linearisedExpectedReports.groupBy( _.key )
            reportsByRule.groupBy( ReportKey(_) ).map {
              case (key,reports) =>
                //logger.info(s"found ${reports.size} for reportKey ${key}")
                expectedMap.get(key) match {
                  // There is no expected report => Map to UnexpectedReport
                  case None =>
                    //logger.info(s"no expected for reportKey ${key}")
                    (reports.groupBy(_.executionTimestamp).map{ case (_,reports) => AggregatedReport(reports.head,UnknownReportType,reports.size)}.toSeq, Seq())
                  case Some(expectedReports) =>

                    //logger.info(s"found ${expectedReports.size} expected for reportKey ${key}")
                    val ReportsToAdd = initializeAggregatedReport.fromExecutionReports(reports,expectedReports)
                    //assert(false)
                    val linearisedAggregated = expectedMap.get(key).getOrElse(Seq()).map{base =>
                      AggregatedReport (
                          base.key
                        , 0
                        , SuccessReportType
                        , new Interval(base.startDate, base.endDate)
                        , ""
                        , SerialInterval(base.serial, base.serial)
                        , None
                    ) }
                    logger.info (s"Merge One started, expected : ${linearisedAggregated.size}, ${ReportsToAdd.size}")
                    val merged = splitMerge.mergeAggregatedReports(linearisedAggregated, ReportsToAdd)
                    logger.info (s"Merge One ended, merge size : ${merged._1.size}, ${merged._2.size}")

                    aggregatedReportsRepository.getAggregatedReportsByDate(ruleId, firstExecutionTimeStamp, lastExecutionTimeStamp) match {
                      case Full(aggregated) => logger.info(s"merge 2 start, ${aggregated.size}")
                        val newMerge = splitMerge.mergeAggregatedReports(aggregated, merged._1)
                        logger.info(s"merge 2 end ${newMerge._1.size}, ${newMerge._2.size}")
                        newMerge
                      case eb:EmptyBox => logger.error(s"could not get aggregatedReport")
                      merged
                   }
                }
            }.toSeq
        }
     res
     }.toSeq

     (res.flatMap(_._1), res.flatMap(_._2))
  }


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
            //logger.info(filteredReports.minBy(_.executionTimestamp.getMillis()).executionTimestamp)
            logger.warn(filteredReports.size.toString)
            if (!reports.isEmpty) {
            val (toSave,toDelete) = newAggregationfromReports(filteredReports)
            //logger.info(toSave.size.toString)
            //logger.warn(toDelete.size.toString)
            aggregatedReportsRepository.saveAggregatedReports(toSave)
            //logger.info(aggregatedReportsRepository.deleteAggregatedReports(toDelete))
            updatesEntriesRepository.setAggregationStatus(reports.map(_._2).max, reports.maxBy(_._2)._1.executionTimestamp)
            }
            else {
              //logger.error("Nothing to do")
            }
          case _ => //logger.error("error with reports")
        }


      case Full(None) => reportsRepository.getReportsWithLowestId match {
        case Full(Some((report,id))) =>

          //logger.info("Just Updating for now")
          updatesEntriesRepository.setAggregationStatus(id, report.executionTimestamp)
        case _ => //logger.error("oulalah")
      }
      case eb:EmptyBox =>   reportsRepository.getReportsWithLowestId match {
        case Full(Some((report,id))) =>

          //logger.info("Just Updating for now")
         //logger.warn( updatesEntriesRepository.setAggregationStatus(id, report.executionTimestamp))
        case _ => //logger.error("oulalah")
      }

    }

  }
}

class InitializeAggregatedReport {
  def fromExecutionReports (reports : Seq[Reports],expectedReports : Seq[LinearisedExpectedReport]  ) = {
    reports.groupBy(_.executionTimestamp).flatMap{
      case (executionTimeStamp,reports) =>
        val headReport = reports.head
        val (expected, unexpected) =  reports.partition(report => expectedReports.exists(_.serial == report.serial))
        val expectedAggregated = expected.map(report => AggregatedReport( report, ReportType(report), 1))
        val unexpectedAggregated = unexpected.map(report => AggregatedReport(report, UnknownReportType, 1))

        unexpectedAggregated ++ expectedAggregated
    }.toSeq
  }
}

class SplitMergeAggregatedReport {  

  def remergeConflict (baseSeq : Seq[AggregatedReport], reportsToMerge : Seq[AggregatedReport], beforeMerge : Option[AggregatedReport], afterMerge : Option[AggregatedReport]) : (Seq[AggregatedReport],Seq[AggregatedReport],Seq[AggregatedReport])= {
    def shouldRemoveBound (bound : Option[AggregatedReport] ) : Boolean = {
      bound match {
        case None => true
        case Some(report) => // Need merge only if the bound is empty and less than reportIntervalSize
          report.received == 0
      }
    }
    //logger.warn(s"remerge ${reportsToMerge.size} into ${baseSeq.size}")
    val reportsToSave = Buffer[AggregatedReport]()
    val reportsToDelete : Buffer[AggregatedReport] = Buffer[AggregatedReport]()
    val unchanged : Buffer[AggregatedReport] = baseSeq.toBuffer
    //Mergebegin
    for {
      reportToMerge <- reportsToMerge

    } yield {
      val updatedReport = 
        baseSeq.filter(baseReport => 
             (baseReport.interval.getEnd isBefore reportToMerge.interval.getStart) 
          && (baseReport.interval.getEnd isAfter (reportToMerge.interval.getStart minusMinutes AGGREGATION_INTERVAL))
        ) match {
        case mergeReports if !mergeReports.isEmpty =>
        //logger.warn("beforeMerge")
        beforeMerge match {
          case Some(beforeReport) =>
            if (beforeReport.received == 0) {
              //logger.warn(mergeReports)
              //logger.error(reportToMerge)
              //logger.info(beforeReport)
              reportsToSave ++= mergeReports.map{report => unchanged -= report
              report.copy(interval = new Interval(report.interval.getStart, reportToMerge.interval.getStart))}
              reportsToDelete += beforeReport
            } else {
              // Nothing to save
            }
            case None =>
              // Nothing to save
        }
        mergeReports.find (mergeReport => mergeReport.status == reportToMerge.status && mergeReport.received == reportToMerge.received ) match {
            case Some(mergeReport) =>
              //logger.info("secondPart")
              val reportMerged =  mergeReport.copy(interval = new Interval(mergeReport.interval.getStart, reportToMerge.interval.getEnd))
              if (reportsToSave.contains(mergeReport)) {
                reportsToSave.update(reportsToSave.indexOf(mergeReport), reportMerged)
              } else {
                reportsToSave += reportMerged
              }
              reportsToDelete += reportToMerge
              reportMerged
            case None =>
              reportToMerge

        }

        case Seq() =>
        // Nothing to save
          reportToMerge
      }

      //logger.info("test")
      baseSeq.filter(baseReport => (baseReport.interval.getStart isBefore (updatedReport.interval.getEnd plusMinutes AGGREGATION_INTERVAL))&& (baseReport.interval.getStart isAfter reportToMerge.interval.getEnd) ) match {
        case mergeReports  =>
        val updatedReportAgain = afterMerge match {
          case Some(afterReport) =>
            //logger.info("afterMerge")
            //logger.info(afterReport)
            //logger.info(updatedReport)
            //logger.info(mergeReports.size.toString)
            if (!mergeReports.isEmpty) {
              if (afterReport.received == 0) {
                val updatedReportAgain = updatedReport.copy(interval = new Interval(updatedReport.interval.getStart, afterReport.interval.getEnd))
                if (reportsToSave.contains(updatedReport)) {
                  reportsToSave.update(reportsToSave.indexOf(updatedReport), updatedReportAgain)
                } else {
                  reportsToSave += updatedReportAgain
                }
                reportsToDelete += afterReport
                updatedReportAgain
              } else {
                updatedReport
              }
            } else {
              reportsToSave += afterReport
              updatedReport
            }
          case None =>
            updatedReport
        }
        //logger.warn(s"after ended, second part $updatedReportAgain")
        mergeReports.find (mergeReport => mergeReport.status == updatedReportAgain.status && mergeReport.received == updatedReportAgain.received ) match {
            case Some(mergeReport) =>
              unchanged -= mergeReport
              val reportMerged =  updatedReportAgain.copy(interval = new Interval(updatedReportAgain.interval.getStart, mergeReport.interval.getEnd))
              if (reportsToSave.contains(updatedReportAgain)) {
                reportsToSave.update(reportsToSave.indexOf(updatedReportAgain), reportMerged)
              } else {
                reportsToSave += reportMerged
              }
              reportsToDelete += mergeReport

            case None =>
              // Nothing to change
        }
      }
    }

   // reportsToSave.foreach(report => //logger.warn(s"toSave $report"))

    //reportsToDelete.foreach(report => //logger.error(s"to delte $report"))
    (reportsToSave.distinct, reportsToDelete.distinct,unchanged.distinct)

  }


  /**
   * Here we try to split an aggregated reports using one base report
   */
  def splitConflict (conflicting : AggregatedReport, report : AggregatedReport) : (Option[AggregatedReport],Seq[AggregatedReport],Option[AggregatedReport]) = {

    def splitBegin (base: AggregatedReport, newReportBegin: DateTime) = {
      val splittedEnd =  base.copy(interval = new Interval(newReportBegin, base.interval.getEnd) , storageId = None)
      val splittedBegin = base.copy(interval = new Interval(base.interval.getStart, newReportBegin))
      (splittedBegin,splittedEnd)
    }

    def splitEnd (base: AggregatedReport, newReportEnd: DateTime) = {
      val splittedBegin =  base.copy(interval = new Interval(base.interval.getStart, newReportEnd))
      val splittedEnd = base.copy(interval = new Interval(newReportEnd, base.interval.getEnd), storageId = None)
      (splittedBegin,splittedEnd)
    }

    val (notConflictingBegin, conflictingEnd) = if ((conflicting.interval.getStart isBefore report.interval.getStart)) {
      val (notConflictingBegin, conflictingEnd) = splitBegin(conflicting,report.interval.getStart)

      (Some(notConflictingBegin), conflictingEnd)
    } else {
      (None,conflicting)
    }

    val (notConflictingEnd, conflictingPart) = if ((conflictingEnd.interval.getEnd isAfter report.interval.getEnd)) {
      val ( conflictingPart, notConflictingEnd) = splitEnd(conflictingEnd,report.interval.getEnd)

      (Some(notConflictingEnd), conflictingPart)
    } else {
      (None,conflictingEnd)

    }

    // Check if there is to much Report
    val newCount = report.received + conflictingPart.received
    val resolvedConflicts = if (newCount > 1) { //we can have at most one report for a key
      // TOO MUCH PUT UNKNOWN WITH NEWCOUNT RECEIVED
      Seq(conflicting.copy(status = UnknownReportType, received = newCount))
    } else {
      // Check if same state
      if (report.status == conflictingPart.status) {
        // Update receive Number and return
        Seq(conflictingPart.copy(received = newCount, message = report.message))
      } else {
        // check if both are empty (should not Happen empty reports whoudl already been treated because they should all get Success status (with 0 reports)
        if (newCount == 0){
          // Both empty report, Wrong report type here, repair it
          Seq(conflicting.copy(status = SuccessReportType))
        } else {
          // Still need to remove empty if there exists, There will still be at least one report in it
          Seq(conflictingPart,report).filter(_.received == 0)
        }
      }
    }

   (notConflictingBegin ,resolvedConflicts, notConflictingEnd)
  }

  def mergeOneAggregatedReport ( base : Seq[AggregatedReport], report : AggregatedReport ) : (Seq[AggregatedReport],Seq[AggregatedReport],Seq[AggregatedReport]) = {
    def createEmptyReport (base: AggregatedReport, begin : DateTime, end: DateTime) = {
      base.copy(interval = new Interval(begin, end))
    }

  logger.warn (s"merge $report in ${base.size}")
    if (!base.isEmpty) {
    // Is the report after all aggregated reports
    if (base.forall(_.interval.getEnd isBefore report.interval.getStart) ) {
      // Get last report from this list
      val maxEndDate : AggregatedReport = base.maxBy(_.interval.getEnd.getMillis())
      val probablyUnchanged = base.filterNot(_ == maxEndDate)
      val (reportsToSave,unchanged) =
        if ((maxEndDate.interval.getEnd isBefore report.interval.getStart) &&(maxEndDate.interval.getEnd isAfter (report.interval.getStart minusMinutes (AGGREGATION_INTERVAL)))) {
        // Check if same report has been received
          logger.info("merge after")
          if (maxEndDate.status == report.status && maxEndDate.received == report.received) {
          // Extend actual report
            //logger.info("extend")
            (Seq(maxEndDate.copy(interval = new Interval(maxEndDate.interval.getStart, report.interval.getEnd))),probablyUnchanged)
          } else {
            // Check if this a no answer
            if (report.received == 0) {
              // Nothing to do, waiting for a new report
              (Seq(report.copy(interval = new Interval(maxEndDate.interval.getEnd, report.interval.getEnd))),base)
            } else {
              // extend previous to beginning of new report and and this new report

              (Seq(maxEndDate.copy(interval = new Interval(maxEndDate.interval.getStart, report.interval.getStart)),report),probablyUnchanged)
            }
          }
        } else {
          (Seq(createEmptyReport(report, maxEndDate.interval.getEnd, report.interval.getStart), report), base)
        }
      (reportsToSave,Seq(), unchanged)
    } else {
      logger.warn("merge middle")
      val (conflictingReports, noConfflictReports) = base.partition(baseReport => baseReport.interval.contains(report.interval) || report.interval.contains(baseReport.interval)  )
      // No conflict, return all reports + the new one
      logger.warn("partition done ")
      //conflictingReports.foreach(conflict => //logger.error(s"conflict $conflict"))
      //noConfflictReports.foreach(conflict => //logger.warn(s"noconflict $conflict"))
      val (toSave,toRemove,unchanged) = if (conflictingReports.size == 0) {
        (Seq(report), Seq(),noConfflictReports)
      } else {
        logger.error(s"treat conflict ${conflictingReports.size}")
        val conflicted = conflictingReports.map(splitConflict(_, report))
        logger.error(s"conflict size is  ${conflicted.flatMap(_._2).size}")
        if (noConfflictReports.isEmpty) {
         logger.warn("emptyConflict")
         val res = ((conflicted :\ Seq[AggregatedReport]()) { case ((begin,reports,end),toSave) => toSave ++ begin ++ reports ++ end}, Seq(), Seq())
         logger.error("end fold")
         res
        }else {
        logger.info ("before remerge")
        val res = conflicted.map(conflict => remergeConflict(base, conflict._2, conflict._1, conflict._3))
                logger.info ("after remerge")
                (res.flatMap(_._1), res.flatMap(_._2),res.flatMap(_._3))

      } }
      (toSave,toRemove,unchanged)
    }
  } else {
    (Seq(report),Seq(),Seq())
  }
  }

  def mergeAggregatedReports ( base : Seq[AggregatedReport], toMerge : Seq[AggregatedReport])  : (Seq[AggregatedReport], Seq[AggregatedReport])= {
    val baseMap = base.groupBy( _.key ).mapValues(_.sortBy(_.interval.getStart.getMillis()))
    val mergeMap = toMerge.groupBy( _.key ).mapValues(_.sortBy(_.interval.getStart.getMillis()))
    val mergingMap = (for {
      key <- (baseMap.keys ++ mergeMap.keys).toSeq.distinct
    } yield {
      key -> (baseMap.getOrElse(key, Seq()),mergeMap.getOrElse(key,Seq()))
    }).toMap
    //logger.error(s"mergeMap $mergingMap")
    val res : Seq[(Seq[AggregatedReport], Seq[AggregatedReport], Seq[AggregatedReport])] = mergingMap.map {
      case (_,(baseReports,Seq())) if baseReports.size > 0 =>
        (Seq[AggregatedReport](),Seq[AggregatedReport](),baseReports)
      case (_,(Seq(),mergeReports)) if mergeReports.size > 0 => (mergeReports :\ (Seq[AggregatedReport](),Seq[AggregatedReport](),Seq[AggregatedReport]())) {
        case (merge, (toSave,delete,unchanged)) => val (newToSave, newDel,newUnchanged) = mergeOneAggregatedReport(toSave++ unchanged,merge)
       ((toSave ++ newToSave).distinct,(delete ++ newDel).distinct, newUnchanged)
       // val res = mergeReports.map(mergeOneAggregatedReport(Seq(),_))
       // (res.flatMap(_._1).distinct, res.flatMap(_._2).distinct)
      }
      // Should not happen
      case (_,(Seq(),Seq())) => (Seq[AggregatedReport](),Seq[AggregatedReport](),Seq[AggregatedReport]())
      case (_,(baseReports,mergeReports)) =>

        (mergeReports :\ (Seq[AggregatedReport](),Seq[AggregatedReport](),base)) {
        case (merge, (toSave,delete,unchanged)) => val (newToSave, newDel,newUnchanged) = mergeOneAggregatedReport((toSave ++ unchanged).distinct,merge)
        //logger.error(s"new ${newToSave}")
       ((toSave ++ newToSave).distinct,(delete ++ newDel).distinct, newUnchanged)
        }
      //  val res = mergeReports.map(mergeOneAggregatedReport(baseReports,_))
       // (res.flatMap(_._1).distinct, res.flatMap(_._2).distinct)
    }.toSeq
    //logger.warn(s"res is $res");
    (res.flatMap(_._1), res.flatMap(_._2))
  }


  /**
   * unfold expected reports to have proper lines
   */
  def lineariseExpectedReport(
      aRuleExpectedReports : RuleExpectedReports
  ) : Seq[LinearisedExpectedReport] = {

    for {
      directivesOnNodes <- aRuleExpectedReports.directivesOnNodes
      directive <- directivesOnNodes.directiveExpectedReports
      nodeId <- directivesOnNodes.nodeIds
       component <- directive.components
       value <-component.componentsValues
    } yield {
      LinearisedExpectedReport(
          ReportKey(
              nodeId
            , aRuleExpectedReports.ruleId
            , directive.directiveId
            , component.componentName
            , value
          )
        , aRuleExpectedReports.serial
        , component.cardinality
        , aRuleExpectedReports.beginDate
        , aRuleExpectedReports.endDate.getOrElse(DateTime.now)
      )
    }
  }
}

//case class MissingReports (expected : Int, received : Int) => Sérialisé en _received_expected pour stocker le nombre

case class LinearisedNodeStatusReport(
    key          : ReportKey
  , serial       : Int
  , reportType   : ReportType
  , message      : String
  , executionDate: DateTime
) extends HashcodeCaching

object LinearisedNodeStatusReport {

  def apply(nodeStatusReport :NodeStatusReport, serial:Int, execDate:DateTime) : Seq[LinearisedNodeStatusReport]= {
    for {
      directive <- nodeStatusReport.directives
      component <- directive.components ++ directive.unexpectedComponents
      keyValue  <- component.componentValues ++ component.unexpectedCptValues
    } yield {
      LinearisedNodeStatusReport(
          ReportKey(
              nodeStatusReport.nodeId
            , nodeStatusReport.ruleId
            , directive.directiveId
            , component.component
            , keyValue.componentValue
          )
        , serial
        , keyValue.cptValueReportType
        , keyValue.message.mkString(";")
        , execDate
      )
    }
  }

  def apply(executionBatch:ExecutionBatch) : Seq[LinearisedNodeStatusReport] =
    executionBatch.getNodeStatus.flatMap(LinearisedNodeStatusReport(_,executionBatch.serial,executionBatch.executionTime))
}


// a class not unlike the AggregatedReports
//TODO: what is its goal ?
case class LinearisedExpectedReport(
    key         : ReportKey
  , serial      : Int
  , cardinality : Int
  , startDate   : DateTime
  , endDate     : DateTime
) extends HashcodeCaching
