/*
*************************************************************************************
* Copyright 2011 Normation SAS
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

package com.normation.rudder.domain.reports.bean

import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.policies.DirectiveId
import scala.collection._
import org.joda.time._
import org.joda.time.format._
import com.normation.rudder.domain.Constants
import com.normation.cfclerk.domain.{Cf3PolicyDraftId}
import com.normation.rudder.domain.reports.ReportComponent
import com.normation.rudder.domain.reports.DirectiveExpectedReports
import com.normation.utils.HashcodeCaching
import scala.collection.mutable.Buffer
import ConfigurationExecutionBatch._
import com.normation.rudder.domain.logger.ReportLogger
import com.normation.rudder.domain.policies.Directive
import com.normation.rudder.domain.reports.ReportComponent
import com.normation.inventory.domain.NodeId

/**
 * An execution batch contains the node reports for a given Rule / Directive at a given date
 * An execution Batch is at a given time <- TODO : Is it relevant when we have several node ?
 * @author Nicolas CHARLES
 */
trait ExecutionBatch {
  val ruleId : RuleId
  val serial : Int // the serial of the rule
  
  val executionTime : DateTime // this is the time of the batch
  
  val directiveExpectedReports : Seq[DirectiveExpectedReports] // the list of policies, list of component and cardinality
  
  val executionReports : Seq[Reports]
  
  val expectedNodeIds : Seq[NodeId]
  
  def getNodeStatus() : Seq[NodeStatusReport]
  
  def getRuleStatus() : Seq[DirectiveRuleStatusReport]
  
  def getSuccessReports() : Seq[Reports] = {
    executionReports.filter(x => x.isInstanceOf[ResultSuccessReport])
  }

  def getRepairedReports() : Seq[Reports] = {
    executionReports.filter(x => x.isInstanceOf[ResultRepairedReport])
  }
  
  /* Warn is temporarly unused*/
  /*
  def getWarnReports() : Seq[Reports] = {
    executionReports.filter(x => x.isInstanceOf[WarnReport])
  }*/
  
  def getErrorReports() : Seq[Reports] = {
    executionReports.filter(x => x.isInstanceOf[ResultErrorReport])
  }

}


object ConfigurationExecutionBatch {
  final val matchCFEngineVars = """.*\$(\{.+\}|\(.+\)).*""".r
  final val replaceCFEngineVars = """\$\{.+\}|\$\(.+\)"""
}

/**
 * The execution batch for a rule, still a lot of intelligence to add within
 * 
 */
class ConfigurationExecutionBatch( 
    val ruleId                  : RuleId
  , val directiveExpectedReports: Seq[DirectiveExpectedReports]
  , val serial                  : Int
  , val executionTime           : DateTime
  , val executionReports        : Seq[Reports]
  , val expectedNodeIds         : Seq[NodeId]
  , val beginDate               : DateTime
  , val endDate                 : Option[DateTime]
) extends ExecutionBatch {  
  
  // A cache of the already computed values
  val cache = scala.collection.mutable.Map[String, Seq[NodeId]]()
   
  val nodeStatus = scala.collection.mutable.Map[String, Seq[NodeStatusReport]]()
  
  /**
   * This is the main entry point to get the detailed reporting
   * It returns a Sequence of NodeStatusReport which gives, for
   * each node, the status and all the directives associated
   *
   */
  def getNodeStatus() : Seq[NodeStatusReport] = {
    nodeStatus.getOrElseUpdate("nodeStatus", 
      (for {
        nodeId <- expectedNodeIds
        val nodeFilteredReports = executionReports.filter(x => (x.nodeId==nodeId))
       
        val directiveStatusReports  = for {
          expectedDirective <- directiveExpectedReports
          val directiveFilteredReports = nodeFilteredReports.filter(x => x.directiveId == expectedDirective.directiveId)
          
          // look for each component
          val componentsStatus = for {
            expectedComponent <- expectedDirective.components
            val componentFilteredReports = directiveFilteredReports.filter(x => x.component == expectedComponent.componentName)
            
            val componentStatusReport = checkExpectedComponentWithReports(
                    expectedComponent
                  , componentFilteredReports
                  , nodeId
            )
          } yield {
            componentStatusReport
          }
          
          val directiveStatusReport = DirectiveStatusReport(
              expectedDirective.directiveId
            , componentsStatus
            , ReportType.getWorseType(componentsStatus.map(x => x.componentReportType))
            , Seq()
          )
          
        } yield {
          directiveStatusReport
        }
        
        val nodeStatusReport = NodeStatusReport(
            nodeId
          , ruleId
          , directiveStatusReports
          , ReportType.getWorseType(directiveStatusReports.map(x => x.directiveReportType))
          , Seq()
        )
      } yield {
        nodeStatusReport
      })
    )
  }
  
  protected[bean] def checkExpectedComponentWithReports(
      expectedComponent : ReportComponent
    , filteredReports   : Seq[Reports]
    , nodeId            : NodeId
  ) : ComponentStatusReport = {
    
    // easy case : No Reports mean no answer
    filteredReports.size match {
      case 0 => 
        val components = for {
          component <- expectedComponent.componentsValues
        } yield {
          ComponentValueStatusReport(
              component
            , getNoAnswerOrPending()
            , List("No reports received.")
            , nodeId
          ) 
        }
        ComponentStatusReport(
            expectedComponent.componentName
          , components
          , getNoAnswerOrPending()
          , List("No reports received.")
          , Seq()
        )
      case _ => 
        // First, filter out all the not interesting reports
        val purgedReports = filteredReports.filter(x => x.isInstanceOf[ResultErrorReport] 
                               || x.isInstanceOf[ResultRepairedReport]  
                               || x.isInstanceOf[ResultSuccessReport]
                               || x.isInstanceOf[UnknownReport])
      
                               
        val components = for {
            componentValue <- expectedComponent.componentsValues
            val (status,message) = checkExpectedComponentStatus(
                              expectedComponent
                            , componentValue
                            , purgedReports
                            , expectedComponent.componentsValues )
        } yield {
          ComponentValueStatusReport(
                componentValue
              , status
              , message
              , nodeId)
        }
        
        // must fetch extra entries
        val unexpectedReports = getUnexpectedReports(
            expectedComponent.componentsValues.toList
          , purgedReports
        )

        unexpectedReports.size match {
          case 0 =>
            ComponentStatusReport(
                expectedComponent.componentName
              , components
              , ReportType.getWorseType(components.map(x => x.cptValueReportType))
              , purgedReports.map(_.message).toList
              , Seq() 
            )

          case _ => // some bad report
            unexpectedReports.foreach{ invalidReport =>
              ReportLogger.warn("Unexpected report for Directive %s, Rule %s generated on %s on node %s, Component is %s, keyValue is %s. The associated message is : %s".format(
                  invalidReport.directiveId
                , invalidReport.ruleId
                , invalidReport.executionTimestamp
                , invalidReport.nodeId
                , invalidReport.component
                , invalidReport.keyValue
                , invalidReport.message))
            }
            val cpvalue = for {
              unexpectedReport <- unexpectedReports
            } yield {
                ComponentValueStatusReport(
                   unexpectedReport.keyValue
                 , UnknownReportType
                 , List("Report should have not been received",unexpectedReport.message)
                 , unexpectedReport.nodeId
                )
            }
             ComponentStatusReport(
                expectedComponent.componentName
              , components
              , UnknownReportType
              , unexpectedReports.map(_.message).toList
              , cpvalue
            )
        }
      
    }
  }
  

  private[this] def returnWorseStatus(
      reports : Seq[Reports]
  ) : ReportType = {
    if (reports.exists(x => x.isInstanceOf[ResultErrorReport])) {
      ErrorReportType
    } else {
      if (reports.exists(x => x.isInstanceOf[UnknownReport])) {
        UnknownReportType
      } else {
        if (reports.exists(x => x.isInstanceOf[ResultRepairedReport])) {
          RepairedReportType
        } else {
          if (reports.exists(x => x.isInstanceOf[ResultSuccessReport])) {
            SuccessReportType
          } else {
            getNoAnswerOrPending()
          }
        }
      }
    }
    
  }
  
  
  /*
   * An utility method that fetch the proper status
   * of a component key. 
   * Parameters :
   * expectedComponent : the expected component (obviously)
   * currentValue : the current keyValue processes
   * filteredReports : the report for that component (but including all keys)
   */
  protected def checkExpectedComponentStatus(
      expectedComponent      : ReportComponent
    , currentValue           : String
    , purgedReports          : Seq[Reports]
    , values            : Seq[String]
  ) : (ReportType,List[String]) = {
    val unexepectedReports = purgedReports.filterNot(value => values.contains(value.keyValue))
    currentValue match {
      case "None" =>
      val filteredReports = purgedReports.filter( x => x.keyValue == currentValue)

      filteredReports.filter( x => x.isInstanceOf[ResultErrorReport]).size match {
          case i if i > 0 => (ErrorReportType,purgedReports.map(_.message).toList)
          case _ => {
            filteredReports.size match {
              case 0 if unexepectedReports.size==0 =>  (getNoAnswerOrPending(),List("Report not received."))
              case 0 =>  (UnknownReportType,List("Report should have been received."))
              case x if x == expectedComponent.cardinality => 
                (returnWorseStatus(filteredReports),filteredReports.map(_.message).toList)
              case _ => (UnknownReportType,List("Unknown report received."))
            }
          }
        }
            
      case matchCFEngineVars(_) => 
        // convert the entry to regexp, and match what can be matched
         val matchableExpected = currentValue.replaceAll(replaceCFEngineVars, ".*")
         val matchedReports = purgedReports.filter( x => x.keyValue.matches(matchableExpected))
         matchedReports.filter( x => x.isInstanceOf[ResultErrorReport]).size match {
           case i if i > 0 => (ErrorReportType,matchedReports.map(_.message).toList)
           case _ => {
            matchedReports.size match {
              case 0 if unexepectedReports.size==0 =>  (getNoAnswerOrPending(),List("Report not received."))
              case 0 =>  (UnknownReportType,List("Report should have been received."))
              case x if x == expectedComponent.componentsValues.filter( x => x.matches(matchableExpected)).size => 
              (returnWorseStatus(purgedReports),matchedReports.map(_.message).toList)
                // all similar cfengine variable are undistinguable
              case _ => (UnknownReportType,List("Unknown report received."))
            }
          }
         }
         
      case _: String => 
          // for a given component, if the key is not "None", then we are 
          // checking that what is have is what we wish
          // we can have more reports that what we expected, because of
          // name collision, but it would be resolved by the total number
        val keyReports =  purgedReports.filter( x => x.keyValue == currentValue)
        keyReports.filter( x => x.isInstanceOf[ResultErrorReport]).size match {
          case i if i > 0 => (ErrorReportType,keyReports.map(_.message).toList)
          case _ => {
            keyReports.size match {
              case 0 if unexepectedReports.size==0 =>  (getNoAnswerOrPending(),List("Report not received."))
              case 0 =>  (UnknownReportType,List("Report should have been received."))
              case x if x == expectedComponent.componentsValues.filter( x => x == currentValue).size => 
                (returnWorseStatus(purgedReports),keyReports.map(_.message).toList)
              case _ => (UnknownReportType,List("Unknown report received."))
            }
          }
        }
    }
  }
  
  /**
   * Retrieve all the reports that should not be there (due to
   * keyValue not present)
   */
  private[this] def getUnexpectedReports(
      keyValues      : List[String]
    , reports        : Seq[Reports]
    ) : Seq[Reports] = {
    keyValues match {
      case Nil => reports
      case head :: tail =>
        head match {
          case matchCFEngineVars(_) =>
            val matchableExpected = head.replaceAll(replaceCFEngineVars, ".*")
            getUnexpectedReports(
                tail
              , reports.filterNot(x => x.keyValue.matches(matchableExpected)) )
          case s: String =>
            getUnexpectedReports(
                tail
              , reports.filterNot(x => x.keyValue == s ) )
        }
    }
  }
  
  /**
   * Utility method to determine if we are in the pending time, or if the node hasn't answered for a long time
   */
  private[this] def getNoAnswerOrPending() : ReportType = {
    if (beginDate.plus(Constants.pendingDuration).isAfter(DateTime.now())) {
      PendingReportType
    } else {
      NoAnswerReportType
    }
  }

  def getRuleStatus() : Seq[DirectiveRuleStatusReport]={
     directiveExpectedReports.map{ directive =>
       val directiveId = directive.directiveId
       val componentReports = getNodeStatus().flatMap{ nodestatus =>
         val directivesStatus = nodestatus.directives.filter(_.directiveId == directiveId)
         getComponentRuleStatus(directiveId,directive.components,directivesStatus)
       }.groupBy(_.component).mapValues { componentReport =>
         val componentName = componentReport.map(_.component).head
         val componentValueReports = componentReport.flatMap(_.componentValues).
           groupBy(_.componentValue).mapValues{ componentValueReport =>
             val value = componentValueReport.map(_.componentValue).head
             val cptValueReportType = ReportType.getWorseType(componentValueReport.map(_.cptValueReportType))
             val reports = componentValueReport.flatMap(_.reports)
             ComponentValueRuleStatusReport(directiveId,componentName,value,cptValueReportType,reports)
         }.toSeq.map(_._2)
         val componentReportType = ReportType.getWorseType(componentReport.map(_.componentReportType))
         ComponentRuleStatusReport(directiveId,componentName,componentValueReports,componentReportType)
       }.toSeq.map(_._2)
       val reportType = ReportType.getWorseType(componentReports.map(_.componentReportType))
       DirectiveRuleStatusReport(directiveId,componentReports,reportType)
    }
  }

  def getComponentRuleStatus(directiveid:DirectiveId, components:Seq[ReportComponent], directive:Seq[DirectiveStatusReport]) : Seq[ComponentRuleStatusReport]={
     components.map{ component =>
       val id = component.componentName
       val componentvalues = directive.flatMap{ nodestatus =>
         val components = nodestatus.components.filter(_.component==id)
         getComponentValuesRuleStatus(directiveid,id,component.componentsValues,components) ++
         getUnexpectedComponentValuesRuleStatus(directiveid,id,components.flatMap(_.unexpectedCptValues))
       }
          val reportType =   if (componentvalues.map(_.cptValueReportType).contains(UnknownReportType))
            UnknownReportType
         else
           ReportType.getWorseType(componentvalues.map(_.cptValueReportType))
       ComponentRuleStatusReport(directiveid,id,componentvalues,reportType)
     }
 }

 def getComponentValuesRuleStatus(directiveid:DirectiveId, component:String, values:Seq[String], components:Seq[ComponentStatusReport]) : Seq[ComponentValueRuleStatusReport]={
     values.map{
       value =>
         val componentValues = components.flatMap(_.componentValues.filter(_.componentValue==value))
         val nodes = componentValues.map(value => (value.nodeId,value.cptValueReportType,value.message))
         val reports = ReportType.getWorseType(nodes.map(_._2))
         ComponentValueRuleStatusReport(directiveid,component,value,reports,nodes)
     }
 }

 def getUnexpectedComponentValuesRuleStatus(directiveid:DirectiveId, component:String, values:Seq[ComponentValueStatusReport]) : Seq[ComponentValueRuleStatusReport]={
     values.map{
       value =>
         val nodes = Seq((value.nodeId,value.cptValueReportType,value.message))
         val reports = value.cptValueReportType
         ComponentValueRuleStatusReport(directiveid,component,value.componentValue,reports,nodes)
     }
 }

}

/**
 * For a component value, store the report status
 */
case class ComponentValueStatusReport(
    componentValue 		: String
  , cptValueReportType: ReportType
  , message           : List[String]
  , nodeId				    : NodeId
)

/**
 * For a component, store the report status, as the worse status of the component
 * Or error if there is an unexpected component value
 */
case class ComponentStatusReport(
    component           : String
  , componentValues		  : Seq[ComponentValueStatusReport]
  , componentReportType : ReportType
  , message             : List[String]
  , unexpectedCptValues : Seq[ComponentValueStatusReport]
)


case class DirectiveStatusReport(
    directiveId          : DirectiveId
  , components			     : Seq[ComponentStatusReport]
  , directiveReportType  : ReportType
  , unexpectedComponents : Seq[ComponentStatusReport]
)

case class NodeStatusReport(
    nodeId               : NodeId
  , ruleId               : RuleId
  , directives			     : Seq[DirectiveStatusReport]
  , nodeReportType		   : ReportType
  , unexpectedDirectives : Seq[DirectiveStatusReport]
)



sealed trait RuleStatusReport {
  
  def nodesreport  : Seq[(NodeId,ReportType,List[String])]

  def computeCompliance : Option[Int] = {
    if (nodesreport.size>0){
      val reportsSize = nodesreport.size.toDouble
      Some((nodesreport.map(report => report._2 match {
      case SuccessReportType => 1
      case _ => 0
    }):\ 0)((res:Int,value:Int) => res+value) * 100 / reportsSize).map{ res =>
      BigDecimal(res).setScale(0,BigDecimal.RoundingMode.HALF_UP).toInt
      }
    }
    else
      None
  }
}

case class ComponentValueRuleStatusReport(
    directiveid         : DirectiveId
  , component           : String
  , componentValue      : String
  , cptValueReportType  : ReportType
  , reports             : Seq[(NodeId,ReportType,List[String])]
) extends RuleStatusReport {
  
  override val nodesreport = reports  
}

case class ComponentRuleStatusReport (
    directiveid         : DirectiveId
  , component           : String
  , componentValues     : Seq[ComponentValueRuleStatusReport]
  , componentReportType : ReportType
) extends RuleStatusReport {
  
  override val nodesreport = componentValues.flatMap(_.nodesreport)
  
  override def computeCompliance =
   if (componentValues.size>0){
     Some((componentValues.map(_.computeCompliance.getOrElse(0))
         :\ 100)((res:Int,value:Int) => if(value>res)res else value))
   }
    else
      None
}

case class DirectiveRuleStatusReport(
    directiveId          : DirectiveId
  , components           : Seq[ComponentRuleStatusReport]
  , directiveReportType  : ReportType
) extends RuleStatusReport {
  
  override val nodesreport = components.flatMap(_.nodesreport)
  
  override def computeCompliance =
   if (components.size>0){
     Some((components.map(_.computeCompliance.getOrElse(0))
         :\ 100)((res:Int,value:Int) => if(value>res)res else value))
   }
    else
      None
}