package com.normation.rudder.reports.aggregation

import org.joda.time.Duration
import com.normation.rudder.domain.reports.bean.Reports._
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import java.sql.Timestamp
import org.joda.time.Interval

object AggregationConstants {

  val AGGREGATION_STATUS = "aggregationStatus"

  // a no answer time is 10 minutes
  val NoAnswerTime = new Duration(10*60*1000)


  val NOANSWERKEY = "NoAnswer"
  val ERRORKEY = RESULT_ERROR

  //the date-time format for reports
  val DATETIME_FORMAT = "yyyy-MM-dd"
  val DATETIME_PARSER = DateTimeFormat.forPattern(DATETIME_FORMAT)

  /**
   * The expected time between two execution of the agent on a given machine.
   * It's just expected, as a user can clearly start cf-agent at any time, and
   * two runs may have more than that intervalle between run (pathological
   * example: if a run takes more than RUN_INTERVALL to finished, then the time
   * between that one and the precedent is more than RUN_INTERVALL).
   *
   * Time is in seconds
   */
  val RUN_INTERVAL : Int = 5*60

  /**
   * Duration is in seconds
   */
  val AGGREGATION_INTERVAL : Int = RUN_INTERVAL + RUN_INTERVAL / 2

  /**
   * minimum time under which to date are "equals"
   * that allows to not take into accounts glipse that may
   * happens time difficulties
   * moreover, we really don't expect two agent run to be
   * nearer than that time
   *
   * Unit: seconds
   */
  val RESOLUTION = 10

  /**
   * Define if two date coincides
   */
  def coincide(t1: DateTime, t2: DateTime) : Boolean = {
    (new Interval(
        t1.minusSeconds(RESOLUTION)
      , t1.plusSeconds(RESOLUTION))
    ).contains(t2)
  }

  implicit def toTimeStamp(d:DateTime) : Timestamp = new Timestamp(d.getMillis)
}