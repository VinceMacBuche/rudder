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