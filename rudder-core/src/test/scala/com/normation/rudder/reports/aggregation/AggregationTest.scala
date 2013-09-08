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
import com.normation.rudder.domain.reports.bean.ErrorReportType



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
 
  val now = DateTime.now()

  
  // Execution Reports
  
  val nowReport   : ExecutionReport = ExecutionReport(now, SuccessReportType, 12, "")
  val afterReport : ExecutionReport = ExecutionReport(now plusMinutes(5), SuccessReportType, 12, "")
  val errorReport : ExecutionReport = ExecutionReport(now, ErrorReportType, 12, "")

  // Expected Rule
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
    , Some(now plusHours(1))
  )
  
  // ReportKey and other utils
  val key = ReportKey(NodeId("one"), RuleId("cr"), DirectiveId("policy"), "component", "value")
  val msg = ""
  val serials = SerialInterval(12,12)


  // Aggregation Report
  val baseReport : AggregatedReport = AggregatedReport (
      key
    , 1
    , SuccessReportType
    , (now.minusMinutes(5), now)
    , msg
    , serials
    , Some(42)
  )
  val aggregationReport = AggregationReport(baseReport)

  
  val oldReport : AggregationReport = AggregationReport (

      (now.minusMinutes(30),now.minusMinutes(6) )
    , SuccessReportType
    , Some(42)
    , 1
    , serials
    , ""
  )
  
  val missing : AggregationReport = AggregationReport (

      (now.minusMinutes(5),now )
    , SuccessReportType
    , Some(42)
    , 0
    , serials
    , ""
  )
  
  val newReport : AggregationReport = AggregationReport (

      (now.plusMinutes(6),now.plusMinutes(30) )
    , SuccessReportType
    , Some(42)
    , 1
    , serials
    , ""
  )


 val unitAggregation = new UnitAggregationService()


  val start = ARStart(now,SuccessReportType, None, 1, SerialInterval(12,12), "")
  val endingTime = now plusSeconds(RUN_INTERVAL)

  val execSeq = unitAggregation.buildExecutionSequence(Set(AgentExecution(now),AgentExecution(now minusMinutes(5)),AgentExecution(now plusMinutes(5))))
  val oldExecSeq = unitAggregation.buildExecutionSequence(Set(AgentExecution(now minusMinutes(30)),AgentExecution(now),AgentExecution(now minusMinutes(5)),AgentExecution(now plusMinutes(5))))

  val newAR = unitAggregation.createNewAggregatedReports(Seq(nowReport), Seq(ruleExpectedReport), execSeq)
  val newARs = unitAggregation.createNewAggregatedReports(Seq(nowReport,afterReport), Seq(ruleExpectedReport), execSeq)
  val errorAR = unitAggregation.createNewAggregatedReports(Seq(errorReport), Seq(ruleExpectedReport), execSeq)
  
  "unit Aggregation" should {
 
  val result =
      Seq(AggregationReport(new Interval(now, now.plusSeconds(RUN_INTERVAL)), SuccessReportType , None, 1, SerialInterval(12,12), ""))
    "transform a result into an aggregated Report" in {
      unitAggregation.toAR(start, endingTime ) === result.head
    }

    "transform a gap into nothing" in {
      unitAggregation.toAR(Gap(now), endingTime ) === result.head.copy ( nbReceived = 0)
    }
  val newAR = unitAggregation.createNewAggregatedReports(Seq(nowReport), Seq(ruleExpectedReport), execSeq)
 
    "create an aggregationReport from a Report and an expectedReport " in {
      newAR must haveTheSameElementsAs( result)
    }
  
  val result2 = result :+
   AggregationReport(new Interval(now plusMinutes(5), now plusMinutes(5) plusSeconds(RUN_INTERVAL)), SuccessReportType , None, 1, SerialInterval(12,12), "")
  "create two aggregationReport from 2 Report and an expectedReport " in {
      newARs must haveTheSameElementsAs( result2)
    }


  }

  
   val existingReports = unitAggregation.normalizeExistingAggregatedReport(Set(aggregationReport), execSeq)
   
      val oldReports = unitAggregation.normalizeExistingAggregatedReport(Set(oldReport), execSeq.copy (instants = thiinstants + oldReport.interval.getStart))
      
  "normalize aggregation report" should {

    "normalize correcly one report" in {
      existingReports.intervalStarts must haveTheSameElementsAs(Seq(ARStart(now minusMinutes(5),SuccessReportType,Some(42),1,SerialInterval(12,12), ""),Gap(now)  ))
    }
    
    "t" in {
      oldReports.intervalStarts must haveTheSameElementsAs(Seq(ARStart(now minusMinutes(5),SuccessReportType,Some(42),1,SerialInterval(12,12), ""),Gap(now),Gap(now plusMinutes(5))))
    }
  }


  val splitOne = unitAggregation.splitExisting(existingReports, newAR)
  val splitTwo = unitAggregation.splitExisting(existingReports, newARs)
  "splitExisting" should {

    "split one aggregatedreports" in {
      splitOne.intervalStarts must haveTheSameElementsAs(Seq(ARStart(now minusMinutes(5),SuccessReportType,Some(42),1,SerialInterval(12,12), ""),Gap(now),Gap(now plusSeconds(RUN_INTERVAL))))
    }
    
    "split two aggregatedreports" in {
      splitTwo.intervalStarts must haveTheSameElementsAs(Seq(ARStart(now minusMinutes(5),SuccessReportType,Some(42),1,SerialInterval(12,12), ""),Gap(now),Gap(now plusSeconds(RUN_INTERVAL)),Gap(now plusMinutes(5) plusSeconds(RUN_INTERVAL))))
    }
  }


  "unitMerge" should {
    val emptyStart = ARStart(now, SuccessReportType, None, 0, SerialInterval(12,12), "")
    
    unitAggregation.unitMerge(emptyStart, errorAR.head) === ARStart(now minusMinutes(5),ErrorReportType,None,1,SerialInterval(12,12),"")
    
  }
  
  "merge update align" should  {

    
    "merge one aggregatedreports" in {
      val res = unitAggregation.mergeUpdateStatus(splitOne, newAR)
          
      res.intervalStarts must haveTheSameElementsAs( Set(ARStart(now minusMinutes(5),SuccessReportType,Some(42),1,SerialInterval(12,12), ""),ARStart(now,SuccessReportType,None,1,SerialInterval(12,12),""), Gap(now plusSeconds(RUN_INTERVAL))))
    }
    
    "merge two aggregatedreports" in {
      val res = unitAggregation.mergeUpdateStatus(splitTwo, newARs)
          
      res.intervalStarts must haveTheSameElementsAs( 
          Set(
              ARStart(now minusMinutes(5),SuccessReportType,Some(42),1,SerialInterval(12,12), "")
            , ARStart(now,SuccessReportType,None,1,SerialInterval(12,12),"")
            , ARStart(now plusMinutes(5),SuccessReportType,None,1,SerialInterval(12,12),"")
            , Gap(now  plusMinutes(5) plusSeconds(RUN_INTERVAL))))
    }
  }

  "interval containing" should {
    splitOne.getInvervalContaining(now) === (splitOne.intervalStarts.tail.head,splitOne.intervalStarts.tail.tail.head)
  }

  val aggregate = unitAggregation.updateAggregatedReports(Seq(nowReport), Seq(ruleExpectedReport), Set(AgentExecution(now minusMinutes(5))), Set(aggregationReport))
  
  val aggregate2 = unitAggregation.updateAggregatedReports(Seq(nowReport,afterReport), Seq(ruleExpectedReport), Set(AgentExecution(now minusMinutes(5))), Set(aggregationReport))
  
    
  val aggregateMiddle = unitAggregation.updateAggregatedReports(Seq(nowReport), Seq(ruleExpectedReport), Set(AgentExecution(now minusMinutes(5)),AgentExecution(now minusMinutes(30)),AgentExecution(now plusMinutes(6))), Set(oldReport,newReport))
  "complete aggregation" should {

    "add a report after the end" in {
      aggregate.toSave must haveTheSameElementsAs(Set(AggregationReport(new Interval(now minusMinutes(5),now plusMinutes(5)),SuccessReportType,Some(42),1,SerialInterval(12,12),"")))


    }
    
    "extend with 2 report after the end" in {
      aggregate2.toSave must haveTheSameElementsAs(
          Set(
              AggregationReport(new Interval(now minusMinutes(5),now plusMinutes(10)),SuccessReportType,Some(42),1,SerialInterval(12,12),"")))


    }
    
    "extend in the middle" in {
      aggregateMiddle.toSave must haveTheSameElementsAs(
          Set(
              AggregationReport(new Interval(now minusMinutes(5),now plusMinutes(10)),SuccessReportType,Some(42),1,SerialInterval(12,12),"")))


    }

  }

  "extends interval" should {

  }
}
