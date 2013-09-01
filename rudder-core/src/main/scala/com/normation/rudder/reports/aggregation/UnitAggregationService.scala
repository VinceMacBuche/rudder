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

import scala.collection.SortedMap
import scala.collection.SortedSet
import org.joda.time.DateTime
import org.joda.time.Interval
import com.normation.rudder.reports.aggregation.AggregationConstants._
import scala.collection.Iterator
import com.normation.rudder.domain.reports.bean.ReportType
import com.normation.rudder.batch.AggregateReports
import net.liftweb.common.Loggable
import com.normation.rudder.domain.reports.bean.Reports
import com.normation.rudder.domain.reports.RuleExpectedReports
import net.liftweb.common.Box
import com.normation.rudder.domain.reports.bean.UnknownReportType

/**
 * Representation of the moment when an agent ran
 */
final case class AgentExecution(timestamp: DateTime)

/**
 * Agent report, with only the interesting parts for us
 */
final case class RunReport(timestamp: DateTime, status: ReportType, serial: Int)


/**
 * A class that only hold interesting parts of aggregated
 * report for unit aggregation:
 * - not the key
 * - only interval and optional existing db key
 */
final case class AR(
    interval : Interval
  , status   : ReportType
  , storageId: Option[Long]
) {

  val startPoint = ARStart(interval.getStart, status, storageId)
  val endGap = Gap(interval.getEnd)
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
          //we are building the intrvalle for the first, the second is the
          //end edge
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
  val start: DateTime
  def withStart(t: DateTime) : StartingTime
}
final case class Gap(start: DateTime) extends StartingTime {
  override def withStart(t: DateTime) = Gap(t)
}
final case class ARStart(
    start    : DateTime
  , status   : ReportType
  , storageId: Option[Long]
) extends StartingTime {
  override def withStart(t: DateTime) = this.copy(start = t)
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
    intervalStarts.map( _.start ).contains(t)
  }

  /**
   * Get the starting point that contains
   * the given time.
   * If a starting point as the same date, it's
   * that one that is returned.
   * If t if before the first starting point,
   * a new Gap is created starting at that point.
   */
  def getPreviousStartingPoint(t: DateTime) : StartingTime = {

    ((Gap(t):StartingTime)/:intervalStarts)( (previous, current) =>
      if(current.start.isAfter(t)) previous
      else current
    )
  }

  /**
   * Add or replace the starting point with the same datetime
   */
  def replace(point: StartingTime) : AggregatedReports = {
    new AggregatedReports(intervalStarts.filterNot( _.start == point.start) + point)
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
      if(execSeq.instants.contains(t.start)) t
      else execSeq.instants.find(i => coincide(t.start, i)) match {
        case Some(i) => t.withStart(i)
        case None =>
          val i = (execSeq.instants.toSeq
            .map(i => (i, Math.abs(t.start.getMillis - i.getMillis) ))
            .sortBy( _._2 )
            .head._1
          )
          t.withStart(i)
      }
    }

    new AggregatedReports(normalized)
  }
}

object AggregatedReports extends Loggable {
  implicit def startingTimeOrdering: Ordering[StartingTime] = Ordering.fromLessThan(_.start isBefore _.start)

  def apply(reports: Seq[AR]) : AggregatedReports = {

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
        } else if(p1.interval.getEnd.isAfter(p2.interval.getStart)) {
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
    }.toSeq

    AggregatedReports(SortedSet() ++ seq)
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
 *    - extends 0-lenght interval to match execution
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
class UnitAggregationService {

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
  def normalizeExistingAggregatedReport(reports: Set[AR], execSeq: ExecutionSequence): AggregatedReports = {
    val aggregatedReports = AggregatedReports(reports.toSeq)
    aggregatedReports.normalizeWith(execSeq)
  }

  /**
   * Build new expected reports.
   * They are normalized (and aligned) with execSeq by construction.
   *
   * Hypothesis:
   * - expected reports are sorted by beginDate asc
   * - both expected reports and execSeq endpoints are over each runReport timestamp
   *   (i.e: we actually got all the date we needed to work)
   */
  def createNewAggregatedReports(runReports: Seq[RunReport], expectedReports: Seq[RuleExpectedReports], execSeq: ExecutionSequence) : Seq[AR] = {

    def createOne(report: RunReport) : AR = {
      //find the corresponding expected report
      val status = for {
        //no expected means that the report was not exepected :)
        expected <- expectedReports.takeWhile(expect => report.timestamp.isBefore(expect.beginDate)).lastOption
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

      AR(
          interval
        , status.getOrElse(UnknownReportType)
        , None
      )
    }

    runReports.map(createOne(_))
  }


  /**
   * That method add new startpoint where needed in existing reports
   */
  def splitExisting(aggregatedReports: AggregatedReports, newReports: Seq[AR]): AggregatedReports = {
    (aggregatedReports/:newReports.flatMap(r => Seq(r.startPoint.start, r.endGap.start))){ case (agg, instant) =>
      if(agg.isDefinedAt(instant)) agg
      else {
        val newPoint = agg.getPreviousStartingPoint(instant) match {
          case Gap(_) => Gap(instant)
          case ARStart(start, status, _) => ARStart(instant, status, None)
        }
        agg.replace(newPoint)
      }
    }
  }

  /**
   * Merge new aggregated report with existing one.
   * Hypothesis: a splitExisting was done, so that each aggregated report has
   * exactly both its start point and its end point defined as a point of
   * the aggregated reoport.
   */
  def mergeUpdateStatus() = ???

}

