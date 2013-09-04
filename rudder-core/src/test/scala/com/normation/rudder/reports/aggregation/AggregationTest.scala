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

import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._
import com.normation.rudder.domain.reports.bean.Reports
import com.normation.rudder.domain.reports._
import com.normation.rudder.domain.policies._
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.reports.bean.ResultSuccessReport
import org.joda.time.{ DateTime, Interval }
import com.normation.rudder.domain.reports.bean.SuccessReportType
import java.sql.Timestamp



/**
 *
 */
@RunWith(classOf[JUnitRunner])
class AggregationTest extends Specification {
  import AggregationConstants._

  private implicit def str2DirectiveId(s:String) = DirectiveId(s)
  private implicit def str2RuleId(s:String) = RuleId(s)
  private implicit def str2nodeId(s:String) = NodeId(s)
  private implicit def tuple2ToInterval(t2: Tuple2[DateTime, DateTime]) : Interval = new Interval(t2._1, t2._2)

  val aggregation = new SplitMergeAggregatedReport()
  val initialization = new InitializeAggregatedReport()

  val now = DateTime.now()

  val report: Reports  = ResultSuccessReport(now, "cr", "policy", "one", 12, "component", "value",now, "message")

  val reportComponent = ReportComponent (
      "component"
    , 1
    , Seq("value")
    , Seq()
  )
  val directiveExpected = DirectiveExpectedReports(
      DirectiveId("policy")
    , Seq(reportComponent)
  )
  val directivesOnNodes = DirectivesOnNodes (
      1
    , Seq(NodeId("one"))
    , Seq(directiveExpected)
  )
  val ruleExpectedReport = RuleExpectedReports (
      RuleId("cr")
    , 12
    , Seq(directivesOnNodes)
    , now minusHours(1)
    , None
  )
  val key = ReportKey(report)
  val msg = ""
  val serials = SerialInterval(12,12)

  val expectedReport = LinearisedExpectedReport(
      key
    , serial = 12
    , cardinality = 1
    , startDate = now minusMinutes(10)
    , endDate = now plusMinutes(10)
  )

  val reportToAdd : AggregatedReport = AggregatedReport(report, SuccessReportType, 1)


  val baseReport : AggregatedReport = AggregatedReport (
      key
    , 0
    , SuccessReportType
    , (now.minusMinutes(5), now.plusMinutes(5))
    , msg
    , serials
    , Some(42)
  )

  val aggregationReport = AggregationReport(baseReport)


 val unitagg = new UnitAggregationService()

  val executionReport = ExecutionReport(report.executionTimestamp, SuccessReportType, report.serial, "")

  val gap = Gap(now)

  val start = ARStart(now,SuccessReportType, None, 1, SerialInterval(12,12), "")
  val endingTime = now plusSeconds(RUN_INTERVAL)

  val execSeq = unitagg.buildExecutionSequence(Set(AgentExecution(now)))

  val existingReports = unitagg.normalizeExistingAggregatedReport(Set(aggregationReport), execSeq)

  val newARs = unitagg.createNewAggregatedReports(Seq(executionReport), Seq(ruleExpectedReport), execSeq)

  "unit Aggregation" should {
    val result =
      Some(AggregationReport(new Interval(now, now.plusSeconds(RUN_INTERVAL)), SuccessReportType , None, 1, SerialInterval(12,12), ""))

    "transform a result into an aggregated Report" in {
      unitagg.toAR(start, endingTime ) === result
    }

    "transform a gap into nothing" in {
      unitagg.toAR(gap, endingTime ) === None
    }

    "create an aggregatedReport from a Report and an expectedReport " in {
      newARs must haveTheSameElementsAs( result.toSeq)
    }


  }

  "normalize aggregation report" should {

    "normalize correcly one report" in {
      existingReports.intervalStarts must haveTheSameElementsAs(Seq(Gap(now)))
    }
  }


val split = unitagg.splitExisting(existingReports, newARs)

  "splitExisting" should {

   "split one aggregatedreports" in {
   split.intervalStarts must haveTheSameElementsAs(Seq(Gap(now),Gap(now plusSeconds(RUN_INTERVAL))))
   }
  }


  "merge update align" should  {

    val res = unitagg.mergeUpdateStatus(split, newARs)

    res.intervalStarts must haveTheSameElementsAs( Set(ARStart(now,SuccessReportType,None,1,SerialInterval(12,12),""), Gap(now plusSeconds(RUN_INTERVAL)))

)
  }

  "interval containing" should {
    split.getInvervalContaining(now) === (split.intervalStarts.head,split.intervalStarts.tail.head)
  }

}
