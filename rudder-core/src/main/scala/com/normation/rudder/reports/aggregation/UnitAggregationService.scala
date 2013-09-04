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

import scala.Option.option2Iterable
import scala.collection.SortedSet
import org.joda.time.DateTime
import org.joda.time.Interval
import com.normation.rudder.domain.reports.RuleExpectedReports
import com.normation.rudder.domain.reports.bean.ReportType
import com.normation.rudder.domain.reports.bean.UnknownReportType
import com.normation.rudder.reports.aggregation.AggregationConstants.AGGREGATION_INTERVAL
import com.normation.rudder.reports.aggregation.AggregationConstants.RESOLUTION
import com.normation.rudder.reports.aggregation.AggregationConstants.RUN_INTERVAL
import com.normation.rudder.reports.aggregation.AggregationConstants.coincide
import net.liftweb.common.Loggable
import com.normation.rudder.domain.reports.bean.Reports

/**
 * Representation of the moment when an agent ran
 */
final case class AgentExecution(timestamp: DateTime)

/**
 * Agent report, with only the interesting parts for us
 */
final case class ExecutionReport(timestamp: DateTime, status: ReportType, serial: Int, message: String)

object ExecutionReport {
  def apply (report : Reports) : ExecutionReport = {
    ExecutionReport (
        report.executionTimestamp
      , ReportType(report)
      , report.serial
      , report.message
    )
  }
}


/**
 * A class that only hold interesting parts of aggregated
 * report for unit aggregation:
 * - not the key
 * - only interval and optional existing db key
 */
final case class AggregationReport(
    interval   : Interval
  , status     : ReportType
  , storageId  : Option[Long]
  , nbReceived : Int
  , serials    : SerialInterval
  , message    : String
) {

  val startPoint = ARStart(interval.getStart, status, storageId, nbReceived, serials, message)
  val endGap = Gap(interval.getEnd)
}

object AggregationReport {
  def apply (aggregatedReport : AggregatedReport) : AggregationReport = {
    AggregationReport(
        aggregatedReport.interval
      , aggregatedReport.status
      , aggregatedReport.storageId
      , aggregatedReport.received
      , aggregatedReport.serials
      , aggregatedReport.message
    )
  }
}


/**
 * The list of each execution, plus utility method to give
 * the corresponding interval
 *
 * Value must be sorted
 *
 * An execution sequence can't be empty
 * (at least, we do know that reports execution time are in it)
 */
final case class ExecutionSequence private (instants: SortedSet[DateTime]) {

  if(instants.isEmpty) throw new IllegalArgumentException("An execution sequence can not be empty")

  val intervals: Seq[Interval] = instants.toSeq.sliding(2).map { s =>
    //we are building interval for the first, the second is the end edge
    val start = s(0)
    val end = if(s.size == 2) s(1) else s(0)

    //take care of the case where the next known run is
    //too far away, for example because the node was down
    //for a period of time
    val maxEnd = start.plusSeconds(AGGREGATION_INTERVAL)
    val normalizedEnd = if(end.isAfter(maxEnd)) maxEnd else end

    new Interval(start, normalizedEnd)
  }.toSeq

}

object ExecutionSequence {
  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  def apply(seq: Iterable[DateTime]): ExecutionSequence = ExecutionSequence(SortedSet[DateTime]() ++ seq)
}


/**
 * That class represent the starting point of
 * an aggregating report of the given type.
 */
sealed trait StartingTime {
  val dateTime: DateTime
  def withStart(t: DateTime) : StartingTime
}
final case class Gap(dateTime: DateTime) extends StartingTime {
  override def withStart(t: DateTime) = Gap(t)
}
final case class ARStart(
    dateTime   : DateTime
  , status     : ReportType
  , storageId  : Option[Long]
  , nbReceived : Int
  , serials    : SerialInterval
  , message    : String
) extends StartingTime {
  override def withStart(t: DateTime) = this.copy(dateTime = t)
}

/**
 * Main data structure representing aggregated reports
 * between to dates.
 *
 * Aggregated reports don't overlaps, but they may not
 * be continuous (Gap can be present, especially at the
 * end).
 */
case class AggregatedReports private ( intervalStarts: SortedSet[StartingTime] ) {
  import AggregatedReports._

  /**
   * Does a startingtime exists for that datetime ?
   */
  def isDefinedAt(t: DateTime) : Boolean = {
    intervalStarts.map( _.dateTime ).contains(t)
  }

  /**
   * Return the interval containing
   * the given time.
   * Interval are inclusive at start point and
   * exclusive at end point.

   * If t if before the first starting point,
   * a new Gap is created starting at that point.
   *
   * If t is after the last point, a new Gap is
   * created at t+1 millisecond, and use as end point.
   */
  def getInvervalContaining(t: DateTime) : (StartingTime, StartingTime) = {
    val init : (StartingTime, StartingTime) = (Gap(t), Gap(t.plusMillis(1)))

    (init/:intervalStarts)( (previous, current) =>
      if(current.dateTime.isBefore(t) || current.dateTime.isEqual(t)) {
        (current, previous._2)
      } else if(previous._2 == init._2) { //we just croos t
        (previous._1, current)
      } else previous
    )
  }

  /**
   * Add or replace the starting point with the same datetime
   */
  def replace(point: StartingTime) : AggregatedReports = {
    new AggregatedReports(intervalStarts.filterNot( _.dateTime == point.dateTime) + point)
  }

  /**
   * Check that all interval endpoints coincide with the know exec seq.
   * For the one that coincide but are not equals, make the equals.
   * For the one that does not coincide
   * TODO: what to do ? For now, find the nearest exec time, and take that;
   * Certainly a lot of things to validate:
   * - what happen to "near" reports (check consistency after/before)
   * - ...
   */
  def normalizeWith(execSeq: ExecutionSequence) : AggregatedReports = {
    val normalized = intervalStarts.map { t =>
      if(execSeq.instants.contains(t.dateTime)) t
      else execSeq.instants.find(i => coincide(t.dateTime, i)) match {
        case Some(i) => t.withStart(i)
        case None =>
          val i = (execSeq.instants.toSeq
            .map(i => (i, Math.abs(t.dateTime.getMillis - i.getMillis) ))
            .sortBy( _._2 )
            .head._1
          )
          t.withStart(i)
      }
    }

    //no that we dealt with all existing aggregated report,
    //we need to deal with missing one.
    //They are found by execution sequence start NOT in normalized.
    //For them, we just take execSeq interval value, as they should be
    //already normalized

    //normally, we won't add overlapping interval that way, because we use the
    //same normalized sequence of run than for the last part, and
    //interval length in the sequence of run are at most one run long

    //the two set are strictly ascendant in time, we can follow them
    val times = normalized.map( _.dateTime)
    val normalized2 = (normalized /: execSeq.intervals) { case (currentAgg, execInterval) =>

      val addStart = if(times.contains(execInterval.getStart)) currentAgg
      else (currentAgg + Gap(execInterval.getStart))

      if(times.contains(execInterval.getEnd)) addStart
      else (addStart + Gap(execInterval.getEnd))
    }


    new AggregatedReports(normalized2)
  }
}

object AggregatedReports extends Loggable {
  implicit def startingTimeOrdering: Ordering[StartingTime] = Ordering.fromLessThan(_.dateTime isBefore _.dateTime)

  def apply(startingTimes: Iterable[StartingTime]): AggregatedReports = AggregatedReports(SortedSet() ++ startingTimes)

  def apply(reports: Seq[AggregationReport]) : AggregatedReports = {

    val sorted = reports.sortWith( (t1,t2) => t1.interval.getStart.isBefore( t2.interval.getStart) )

    val seq = sorted.sliding(2).flatMap{ s =>

      if(s.size == 1) { //last report: if interval length < RESOLUTION, only one datepoint.
        if(s(0).interval.toDurationMillis < RESOLUTION) {
          Seq(s(0).startPoint)
        } else {
          Seq(s(0).startPoint, s(0).endGap)
        }
      } else {
        val p1 = s(0)
        val p2 = s(1)

        if(coincide(p1.interval.getEnd, p2.interval.getStart)) {
          // OK, the end of p1 coincide with the start of p2
          Seq(p1.startPoint)
        } else {
          if(p1.interval.getEnd.isAfter(p2.interval.getStart)) {
            //that's an error?
            //perhaps a precedent hypothesis on the duration of the interval
            //that was corrected afterward by the reception of the log.
            //In all case, log the problem, and explain that since now,
            //we will take p2.start as end to p1.
            logger.error("Error on aggregated reports: found an interval that ends after the start of another run.") //TODO: better reporting
            Seq(p1.startPoint)
          } else {
            //if p1 is 0-lenght intervalle, extend it
            if(coincide(p1.interval.getStart, p1.interval.getEnd)) {
              //extend how mych ?
              if( p1.interval.getEnd.plusSeconds(AGGREGATION_INTERVAL).isAfter(p2.interval.getStart) ) {
                Seq(p1.startPoint)
              } else { //extends by one run, but expects strange things in the future
                Seq(p1.startPoint, Gap(p1.interval.getStart.plusSeconds(RUN_INTERVAL)))
              }
            } else {
              //case for a gap in our aggregated reports
              Seq(s(0).startPoint, s(0).endGap)
            }
          }
        }
      }
    }.toSeq

    AggregatedReports(seq)
  }
}

/**
 * That class holds the logic to aggregate reports for a
 * given report key.
 * It rely on other services to:
 * - get the datas
 * - sort them by key
 *
 * The global logic of the algo is;
 *
 * 1/ Build the sequence of execution interval
 *    from agent runs
 *    - take care of 3 cases:
 *      - run with a next run < AGGREGATION_INTERVAL
 *      - run with a next run > AGGREGATION_INTERVAL
 *      - run without a next run
 *
 * 2/ Normalize existing aggregated reports
 *    on the execution sequence:
 *    - extends 0-length interval to match execution
 *      intervals one
 *    - check that edges of existing aggregated report
 *      are aligned with edges of execution sequence
 *
 * 3/ From expected reports, execution reports and
 *    execution sequence, build new aggregated
 *    reports
 *
 * 4/ split existing aggregated report according to
 *    new aggregated reports
 *
 * 5/ merge overlapping interval
 *
 * 6/ merge contiguous interval with compatible status
 *
 */
class UnitAggregationService extends Loggable {

  def updateAggregatedReports(
      //the new execution reports to aggregate
      //TODO: should be a seq or a set ?
      newExecutionReports: Seq[ExecutionReport]
      //expected reports on the interval
    , expectedReports: Seq[RuleExpectedReports]
      //a set of know agent run for the processed node
    , agentExecutions: Set[AgentExecution]
      //a set of existing aggregation reports for the
      //interval of new reports.
    , existingAggregationReports: Set[AggregationReport]
  ): Set[AggregationReport] = {

    //just to be sure, add all execution timestamp to the know agent run.
    val allRuns = agentExecutions ++ newExecutionReports.map(r => AgentExecution(r.timestamp))

    val execSeq = buildExecutionSequence(allRuns)
    val existingReports = normalizeExistingAggregatedReport(existingAggregationReports, execSeq)
    logger.info(existingReports)
    val newReports = createNewAggregatedReports(newExecutionReports, expectedReports, execSeq)

    //now, start split/merge with split:
    val splittedExisting = splitExisting(existingReports, newReports)
    //now merge phase 1: update status with new reports
    val mergedReports = mergeUpdateStatus(splittedExisting, newReports)

    //and merge phase 2: extends interval of aggregated reports
    val extendedReports = extendsInterval(mergedReports)

    //finally, transform back our aggregatedReports datastructure to AR
    //last report is built with an interval of length 0
    val points = extendedReports.toSave.intervalStarts.toSeq

    if(points.isEmpty) Set()
    else {
      val even = if(points.size%2 == 0) points else points.dropRight(1)

      val allButTheLast = even.sliding(2).toSeq.flatMap { s => //s.size == 2
        //we are building the AR for the first point, the second only give the end
        toAR(s(0), s(1).dateTime)
      }

      val last = points.last

      Set() ++ allButTheLast ++ toAR(last, last.dateTime).toSeq
    }
  }

  ////////////////////////////
  // Implementation details //
  ////////////////////////////
  /**
   * From a starting point and an ending date, build a aggregated reports
   */
  def toAR(start: StartingTime, end: DateTime) : Option[AggregationReport] = {
    start match {
      case Gap(_) => None
      case ARStart(t, status, storageId, nbReports, serials, message) =>
        Some(AggregationReport(new Interval(t, end), status, storageId, nbReports, serials, message))
    }
  }


  /**
   * Build the sequence of execution interval
   * from agent runs
   *    - take care of 3 cases:
   *      - run with a next run < AGGREGATION_INTERVAL
   *      - run with a next run > AGGREGATION_INTERVAL
   *      - run without a next run
   *
   * Hypothesis on the returned datas:
   * all execution reports timestamp matches an edge
   * of the execution sequence by construction of the
   * execution sequence.
   *
   */
  def buildExecutionSequence(agentExecutions: Set[AgentExecution]): ExecutionSequence = {
    ExecutionSequence(agentExecutions.map(_.timestamp))
  }

  /**
   * That class align existing aggregated report to
   *
   * The return value is correctly sorted by time, without
   * any overlapping intervals.
   */
  def normalizeExistingAggregatedReport(reports: Set[AggregationReport], execSeq: ExecutionSequence): AggregatedReports = {
    val aggregatedReports = AggregatedReports(reports.toSeq)
    logger.warn(aggregatedReports)
    val res = aggregatedReports.normalizeWith(execSeq)
    logger.warn(res)
    res
  }

  /**
   * Build new aggregated reports.
   * They are normalized (and aligned) with execSeq by construction.
   *
   * Hypothesis:
   * - expected reports are sorted by beginDate asc
   * - both expected reports and execSeq endpoints are over each executionReport timestamp
   *   (i.e: we actually got all the date we needed to work)
   */
  def createNewAggregatedReports(executionReports: Seq[ExecutionReport], expectedReports: Seq[RuleExpectedReports], execSeq: ExecutionSequence) : Seq[AggregationReport] = {

    def createOne(report: ExecutionReport) : AggregationReport = {
      //find the corresponding expected report
      val status = for {
        //no expected means that the report was not exepected :)
        expected <- expectedReports.takeWhile(expect => expect.interval.contains(report.timestamp)).lastOption
        serialOk <- if(expected.serial == report.serial) Some(report.status) else None
      } yield {
        serialOk
      }

      //now, find the interval for that report

      val interval = execSeq.intervals.find( i => i.contains(report.timestamp) ) match {
        case None =>
          //something failed miserly here. It seems that our execSeq miss some datapoint.
          if(report.timestamp.isBefore(execSeq.instants.head) || report.timestamp.isAfter(execSeq.instants.last)) {
            throw new IllegalArgumentException("Got a report out of execution sequence bounds: " + report)
          } else {
            //exec sequence misses some datapoints
            val end = report.timestamp.plusSeconds(RUN_INTERVAL)
            execSeq.intervals.find( j => j.contains(end)) match {
              case None => new Interval(report.timestamp, end)
              case Some(x) => new Interval(report.timestamp, x.getStart)
            }
          }
        case Some(i) => i
      }

      //create a new aggregated report.
      AggregationReport(
          interval
        , status.getOrElse(UnknownReportType)
        , None
        , 1
        , SerialInterval(report.serial)
        , report.message
      )
    }

    executionReports.map(createOne(_))
  }


  /**
   * That method add new startpoint where needed in existing reports.
   * New report are created without a storageId, but keep the
   * number of received reports and status
   */
  def splitExisting(aggregatedReports: AggregatedReports, newReports: Seq[AggregationReport]): AggregatedReports = {
    (aggregatedReports/:newReports.flatMap(r => Seq(r.startPoint.dateTime, r.endGap.dateTime))){ case (agg, instant) =>
      if(agg.isDefinedAt(instant)) agg /// and the end ????
      else {
        val newPoint = agg.getInvervalContaining(instant) match {
          case (Gap(_), _) => Gap(instant)
          case (ARStart(start, status, _, i, serials, message) , _) => ARStart(instant, status, None, i, serials, message)
        }
        agg.replace(newPoint)
      }
    }
  }

  /**
   * Merge new aggregated report with existing one.
   * Hypothesis: a splitExisting was done, so that each aggregated report has
   * exactly both its start point and its end point defined as a point of
   * the aggregated report.
   */
  def mergeUpdateStatus(aggregatedReports: AggregatedReports, newReports: Seq[AggregationReport]): AggregatedReports = {
    def areAligned(): Unit = {
      //check hypothesis
      val nonOk = newReports.flatMap { case report =>
        val existing = aggregatedReports.getInvervalContaining(report.interval.getStart)
        if(  existing._1.dateTime != report.interval.getStart
          || existing._2.dateTime != report.interval.getEnd
        ) {
          logger.error(report)
          logger.warn(existing)
          Some("TODO: error message about report not aligned with existing aggregated report")
        } else {
          None
        }
      }
      if(nonOk.nonEmpty) {
        throw new IllegalArgumentException("Found some non aligned report between existing aggregated reports and new one: " + nonOk.mkString("; "))
      }
    }

    areAligned()

    //now, actually merge things
    //new reports may not be in any order, that does not matter
    (aggregatedReports /: newReports) { case (aggregated, report) =>
      val existing = aggregated.getInvervalContaining(report.startPoint.dateTime)
      aggregated.replace(unitMerge(existing._1, report))
    }
  }

  /**
   * Merge an existing aggregated report with a new one.
   * It's the core of the algorithme, where status updates
   * happen!
   *
   * Hypothesis: start time of new and old reports match
   * (end time is not taken into account, it must have been
   * checked before).
   */
  def unitMerge(existingAggregatedReport: StartingTime, newReport: AggregationReport) : StartingTime = {
    existingAggregatedReport match {
      case Gap(t) => ARStart(t, newReport.status, newReport.storageId, newReport.nbReceived, newReport.serials, newReport.message)
      case r@ARStart(t, status, storageId, nbReceived, serials, message) =>
        val total = nbReceived+newReport.nbReceived
        if(total == 0) {
          // ??? well, does it makes sense.
          r
        } else if(total > 1) {
          //thats an unknown status. For a given key, we should always have exactly 0 or 1
          //report for a given time.
          val newSerials = serials.update(newReport.serials)
          val newMessage = s"${message} | ${newReport.message}"
          ARStart(t, UnknownReportType, storageId, total, newSerials, newMessage)
        } else { // total == 1
          //the report with one received report is the good status
          val (newStatus, newSerials, newMessage) = if(nbReceived == 0) (newReport.status, newReport.serials, newReport.message) else (status,serials,message)
          ARStart(t, newStatus, storageId, 1, newSerials, newMessage)
        }
    }
  }


  /**
   * Merge contiguous aggregated reports with the same status
   */
  def extendsInterval(aggregatedReports: AggregatedReports): AggregationResult = {
    if(aggregatedReports.intervalStarts.isEmpty) AggregationResult(aggregatedReports)
    else {
      val init = (Seq[StartingTime](), Option.empty[StartingTime], Seq[StartingTime]())
      val (reports, last, reportstoDelete) = (init /: aggregatedReports.intervalStarts){ case ((processed, optCurrent, toDelete), nextReport) =>
          optCurrent match {
            case None => (processed, Some(nextReport), Seq())
            case Some(current) => //does current and next are meageable ?
              (current, nextReport) match {
                case ( Gap(t), Gap(_) ) => //extends gap
                  (processed, Some(Gap(t)), toDelete)
                case ( r@ARStart(t1, status1, storageId1, nb1, serials1, message1), r2@ARStart(t2, status2, storageId2, nb2, serials2, message2s) ) =>
                  if(status1 == status2 && nb1 == nb2) { //extend ! Try to keep existing storageId if at least one defined
                    val newSerials = serials1.update(serials2)
                    val newDelete =
                      if (storageId1.nonEmpty && storageId2.nonEmpty) {
                        toDelete :+ r2
                      } else {
                        toDelete
                      }

                    (processed, Some(r.copy(storageId = storageId1.orElse(storageId2), serials = newSerials)), newDelete )
                  } else { //don't extend
                    (processed :+ current, Some(nextReport), toDelete)
                  }
                case (_,_) => //not homogeneous, don't extend
                  (processed :+ current, Some(nextReport), toDelete)
              }
          }
      }
      val newStartingPoints = reports ++ last.toSeq
      AggregationResult(AggregatedReports(newStartingPoints), AggregatedReports(reportstoDelete))
    }
  }

}

case class AggregationResult (
    toSave : AggregatedReports
  , toDelete : AggregatedReports
)

object AggregationResult {
  def apply(toSave: AggregatedReports) : AggregationResult = AggregationResult(toSave, AggregatedReports(Set()))
}