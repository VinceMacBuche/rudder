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

package com.normation.rudder.web.components

import com.normation.cfclerk.domain.Technique
import com.normation.cfclerk.services.TechniqueRepository
import com.normation.rudder.domain.policies._
import com.normation.rudder.domain.nodes.NodeGroupId
import com.normation.rudder.domain.policies._
import com.normation.rudder.domain.eventlog.RudderEventActor
import com.normation.rudder.domain.reports.bean._
import com.normation.rudder.domain.nodes.NodeInfo
import com.normation.rudder.repository._
import com.normation.rudder.services.reports.ReportingService
import com.normation.rudder.services.nodes.NodeInfoService
import net.liftweb.http.js._
import JsCmds._
import com.normation.inventory.domain.NodeId
import JE._
import net.liftweb.common._
import net.liftweb.http._
import scala.xml._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import com.normation.rudder.web.model._
import com.normation.utils.StringUuidGenerator
import com.normation.exceptions.TechnicalException
import com.normation.utils.Control.sequence
import com.normation.utils.HashcodeCaching
import com.normation.eventlog.ModificationId
import bootstrap.liftweb.RudderConfig
import net.liftweb.json.JArray
import net.liftweb.json.JsonParser
import net.liftweb.json.JString
import net.liftweb.json.JObject
import net.liftweb.json.JField
import net.liftweb.http.js.JE.JsArray



object RuleGrid {
  def staticInit =
    <head>
      <script type="text/javascript" language="javascript" src="/javascript/datatables/js/jquery.dataTables.js"></script>
      <script type="text/javascript" language="javascript" src="/javascript/rudder/rudder-datatable.js"></script>
      <style type="text/css">
        #actions_zone , .dataTables_length , .dataTables_filter {{ display: inline-block; }}
        .greenCompliance {{ background-color: #CCFFCC }}
        .orangeCompliance  {{ background-color: #FFBB66 }}
        .redCompliance  {{ background-color: #FF6655 }}
        .noCompliance   {{ background-color:#BBAAAA; }}
        .applyingCompliance {{ background-color:#CCCCCC; }}
        .compliance {{ text-align: center; }}
        .statusCell {{font-weight:bold}}
      </style>
    </head>
}

class RuleGrid(
    htmlId_rulesGridZone : String
  , rules : Seq[Rule]
   //JS callback to call when clicking on a line
  , detailsCallbackLink : Option[(Rule,String) => JsCmd]
  , showCheckboxColumn:Boolean = true
  , directiveApplication : Option[DirectiveApplicationManagement] = None
) extends DispatchSnippet with Loggable {

  sealed trait Line { val rule:Rule }


  case class OKLine(
      rule             : Rule
    , compliance       : Option[ComplianceLevel]
    , applicationStatus: ApplicationStatus
    , trackerVariables : Seq[(Directive,ActiveTechnique,Technique)]
    , targets          : Set[RuleTargetInfo]
  ) extends Line with HashcodeCaching

  case class ErrorLine(
      rule:Rule
    , trackerVariables: Box[Seq[(Directive,ActiveTechnique,Technique)]]
    , targets:Box[Set[RuleTargetInfo]]
  ) extends Line with HashcodeCaching

  private[this] val getFullNodeGroupLib = RudderConfig.roNodeGroupRepository.getFullGroupLibrary _
  private[this] val getFullDirectiveLib = RudderConfig.roDirectiveRepository.getFullDirectiveLibrary _
  private[this] val getRuleApplicationStatus = RudderConfig.ruleApplicationStatus.isApplied _

  private[this] val reportingService = RudderConfig.reportingService
  private[this] val getAllNodeInfos  = RudderConfig.nodeInfoService.getAll _
  private[this] val techniqueRepository = RudderConfig.techniqueRepository
  private[this] val categoryRepository  = RudderConfig.roRuleCategoryRepository
  private[this] val categoryService     = RudderConfig.ruleCategoryService


  //used to error tempering
  private[this] val roRuleRepository    = RudderConfig.roRuleRepository
  private[this] val woRuleRepository    = RudderConfig.woRuleRepository
  private[this] val uuidGen             = RudderConfig.stringUuidGenerator


  /////  local variables /////
  private[this] val htmlId_rulesGridId = "grid_" + htmlId_rulesGridZone
  private[this] val htmlId_reportsPopup = "popup_" + htmlId_rulesGridZone
  private[this] val htmlId_modalReportsPopup = "modal_" + htmlId_rulesGridZone
  private[this] val htmlId_rulesGridWrapper = htmlId_rulesGridId + "_wrapper"
  private[this] val tableId_reportsPopup = "popupReportsGrid"



  def templatePath = List("templates-hidden", "reports_grid")
  def template() =  Templates(templatePath) match {
    case Empty | Failure(_,_,_) =>
      throw new TechnicalException("Template for report grid not found. I was looking for %s.html".format(templatePath.mkString("/")))
    case Full(n) => n
  }
  def reportTemplate = chooseTemplate("reports", "report", template)

  def dispatch = {
    case "rulesGrid" => { _:NodeSeq => rulesGrid(getAllNodeInfos(), getFullNodeGroupLib(), getFullDirectiveLib()) }
  }

  def jsVarNameForId(tableId:String) = "oTable" + tableId

  def rulesGridWithUpdatedInfo(popup: Boolean = false, linkCompliancePopup:Boolean = true) = {
    rulesGrid(getAllNodeInfos(), getFullNodeGroupLib(), getFullDirectiveLib(), popup, linkCompliancePopup)
  }


  def selectAllVisibleRules(status : Boolean) : JsCmd= {
    directiveApplication match {
      case Some(directiveApp) =>
        def moveCategory(arg: String) : JsCmd = {
        //parse arg, which have to  be json object with sourceGroupId, destCatId
          try {
            (for {
               JObject(JField("rules",JArray(childs)) :: Nil) <- JsonParser.parse(arg)
               JString(ruleid) <- childs
             } yield {
               RuleId(ruleid)
             }) match {
              case ruleIds =>
                directiveApp.checkRules(ruleIds,status)  match {
                case DirectiveApplicationResult(rules,completeCategories,indeterminate) =>
                  After(TimeSpan(50),JsRaw(s"""
                    ${rules.map(c => s"""$$('#${c.value}Checkbox').prop("checked",${status}); """).mkString("\n")}
                    ${completeCategories.map(c => s"""$$('#${c.value}Checkbox').prop("indeterminate",false); """).mkString("\n")}
                    ${completeCategories.map(c => s"""$$('#${c.value}Checkbox').prop("checked",${status}); """).mkString("\n")}
                    ${indeterminate.map(c => s"""$$('#${c.value}Checkbox').prop("indeterminate",true); """).mkString("\n")}
                  """))
            }
            }
          } catch {
            case e:Exception => Alert("Error while trying to move group :"+e)
          }
        }

        JsRaw(s"""
            var rules = $$.map($$('#grid_rules_grid_zone tr.tooltipabletr'), function(v,i) {return $$(v).prop("id")});
            var rulesIds = JSON.stringify({ "rules" : rules });
            ${SHtml.ajaxCall(JsVar("rulesIds"), moveCategory _)};
        """)
      case None => Noop
    }
  }

  def rulesGrid(
      allNodeInfos: Box[Set[NodeInfo]]
    , groupLib    : Box[FullNodeGroupCategory]
    , directiveLib: Box[FullActiveTechniqueCategory]
    , popup       : Boolean = false
    , linkCompliancePopup:Boolean = true
  ) : NodeSeq = {
    showRulesDetails(popup,rules,linkCompliancePopup, allNodeInfos, groupLib, directiveLib) match {
      case eb:EmptyBox =>
        val e = eb ?~! "Error when trying to get information about rules"
        logger.error(e.messageChain)
        e.rootExceptionCause.foreach { ex =>
          logger.error("Root exception was:", ex)
        }

        <div id={htmlId_rulesGridZone}>
          <div id={htmlId_modalReportsPopup} class="nodisplay">
            <div id={htmlId_reportsPopup} ></div>
          </div>
          <span class="error">{e.messageChain}</span>
        </div>


      case Full(tableData) =>

        // Build refresh function for Rule grid
        val refresh =  SHtml.ajaxCall(JsNull, (s) => {
        val result = for {
            rules <- roRuleRepository.getAll(false)
        } yield {

          val nodeInfo = getAllNodeInfos()
          val groupLib = getFullNodeGroupLib()
          val  directiveLib = getFullDirectiveLib()
          val cmd = showRulesDetails(popup,rules,linkCompliancePopup, nodeInfo, groupLib, directiveLib) match {
            case Full(newData) => s"""table = refreshTable("${htmlId_rulesGridId}",${newData.toJsCmd});"""
            case _ => ""
          }
            Run(cmd)
        }

        result.getOrElse(Run(""))
      })
        val onLoad =
          s"""
      var allcheckboxCallback = function(checked) { ${SHtml.ajaxCall(JsVar("checked"), (in : String) => selectAllVisibleRules(in.toBoolean))}};
      var refresh = function() { ${refresh.toJsCmd} };
      createRuleTable ("${htmlId_rulesGridId}", ${tableData.toJsCmd}, ${showCheckboxColumn}, ${popup}, allcheckboxCallback,refresh);
      createTooltip();
      createTooltiptr();
      $$('#${htmlId_rulesGridWrapper}').css("margin","10px 0px 0px 0px");
      """
    ( <div id={htmlId_rulesGridZone}>
        <div id={htmlId_modalReportsPopup} class="nodisplay">
          <div id={htmlId_reportsPopup} ></div>
        </div>
        <table id={htmlId_rulesGridId} class="display" cellspacing="0"> </table>
        <div class={htmlId_rulesGridId +"_pagination, paginatescala"} >
          <div id={htmlId_rulesGridId +"_paginate_area"}></div>
        </div>
      </div>
    ) ++
    Script(OnLoad(JsRaw(onLoad)))

    }
  }

  private[this] def showRulesDetails(
      popup:Boolean
    , rules:Seq[Rule]
    , linkCompliancePopup:Boolean
    , allNodeInfos: Box[Set[NodeInfo]]
    , groupLib    : Box[FullNodeGroupCategory]
    , directiveLib: Box[FullActiveTechniqueCategory]) : Box[JsArray] = {

    for {
      directivesLib <- directiveLib
      groupsLib     <- groupLib
      nodes         <- allNodeInfos
    } yield {
      val lines = for {
         line <- RulesToLines(directivesLib, groupsLib, nodes, rules.toList)
      } yield {
        LineInfo(line, groupsLib, nodes)
      }
      JsArray(lines)
    }

  }


  private[this] def RulesToLines (
      directivesLib: FullActiveTechniqueCategory
    , groupsLib: FullNodeGroupCategory
    , nodes: Set[NodeInfo]
    , rules : List[Rule]
  ) : List[Line] = { rules.map {
    rule =>
    // we compute beforehand the compliance, so that we have a single big query
    // to the database
    val complianceMap = computeCompliances(rules.toSet)

    val trackerVariables: Box[Seq[(Directive, ActiveTechnique, Technique)]] = {
      sequence(rule.directiveIds.toSeq) { id =>
        directivesLib.allDirectives.get(id) match {
          case Some((activeTechnique, directive)) =>
            techniqueRepository.getLastTechniqueByName(activeTechnique.techniqueName) match {
              case None =>
                Failure(s"Can not find Technique for activeTechnique with name ${activeTechnique.techniqueName} referenced in Rule with ID ${rule.id.value}")
              case Some(technique) =>
                Full((directive, activeTechnique.toActiveTechnique, technique))
            }
          case None => //it's an error if the directive ID is defined and found but it is not attached to an activeTechnique
            val error = Failure(s"Can not find Directive with ID '${id.value}' referenced in Rule with ID '${rule.id.value}'")
            logger.debug(error.messageChain, error)
            error
        }
      }
    }

    val targetsInfo = sequence(rule.targets.toSeq) {
      case json:CompositeRuleTarget =>
        val ruleTargetInfo = RuleTargetInfo(json,"","",true,false)
        Full(ruleTargetInfo)
      case target =>
        groupsLib.allTargets.get(target) match {
          case Some(t) =>
            Full(t.toTargetInfo)
          case None =>
            Failure(s"Can not find full information for target '${target}' referenced in Rule with ID '${rule.id.value}'")
        }
     }.map(x => x.toSet)

     (trackerVariables, targetsInfo) match {
       case (Full(seq), Full(targets)) =>
         val applicationStatus = getRuleApplicationStatus(rule, groupsLib, directivesLib, nodes)
         val compliance =  applicationStatus match {
           case _:NotAppliedStatus =>
             Full(None)
           case _ =>
             complianceMap.getOrElse(rule.id, Failure(s"Error when getting compliance for Rule ${rule.name}"))
         }
         compliance match {
           case e:EmptyBox =>
             ErrorLine(rule, trackerVariables, targetsInfo)
           case Full(value) =>
             OKLine(rule, value, applicationStatus, seq, targets)
         }
       case (x,y) =>
         if(rule.isEnabledStatus) {
           //the Rule has some error, try to disable it
           //and be sure to not get a Rules from a modification pop-up, because we don't want to commit changes along
           //with the disable.
           //it's only a try, so it may fails, we won't try again
           ( for {
             r <- roRuleRepository.get(rule.id)
             _ <- woRuleRepository.update(
                      r.copy(isEnabledStatus=false)
                    , ModificationId(uuidGen.newUuid)
                    , RudderEventActor
                    , Some("Rule automatically disabled because it contains error (bad target or bad directives)")
                  )
           } yield {
             logger.warn(s"Disabling rule '${rule.name}' (ID: '${rule.id.value}') because it refers missing objects. Go to rule's details and save, then enable it back to correct the problem.")
             x match {
               case f: Failure =>
                 logger.warn(s"Rule '${rule.name}' (ID: '${rule.id.value}' directive problem: " + f.messageChain)
               case _ => // Directive Ok!
             }
             y match {
                  case f: Failure =>
                    logger.warn(s"Rule '${rule.name}' (ID: '${rule.id.value}' target problem: " + f.messageChain)
                  case _ => // Group Ok!
             }
           } ) match {
             case eb: EmptyBox =>
               val e = eb ?~! s"Error when to trying to disable the rule '${rule.name}' (ID: '${rule.id.value}') because it's data are unconsistant."
               logger.warn(e.messageChain)
               e.rootExceptionCause.foreach { ex =>
                 logger.warn("Exception was: ", ex)
               }
             case _ => //ok
           }
         }
         ErrorLine(rule, x, y)
      }
    }
  }

  private[this]  def LineInfo(line:Line, groupsLib: FullNodeGroupCategory, nodes: Set[NodeInfo]) : JsObj = {

    val (status,reasons) : (String,Option[String]) =
      line match {
        case line : OKLine =>
          line.applicationStatus match {
            case FullyApplied => ("In application",None)
            case PartiallyApplied(seq) =>
              val why = seq.map { case (at, d) => "Directive " + d.name + " disabled" }.mkString(", ")
              ("Partially applied", Some(why))
            case x:NotAppliedStatus =>
              val isAllTargetsEnabled = line.targets.filter(t => !t.isEnabled).isEmpty
              val nodeSize = groupsLib.getNodeIds(line.rule.targets, nodes).size
              val conditions = {
                Seq( ( line.rule.isEnabled            , "Rule disabled" )
                   , ( line.trackerVariables.size > 0 , "No policy defined")
                   , ( isAllTargetsEnabled            , "Group disabled")
                   , ( nodeSize!=0                    , "Empty groups")
                ) ++
                line.trackerVariables.flatMap {
                  case (directive, activeTechnique,_) =>
                    Seq( ( directive.isEnabled , "Directive " + directive.name + " disabled")
                       , ( activeTechnique.isEnabled, "Technique for '" + directive.name + "' disabled")
                    )
                }
              }
              val why =  conditions.collect { case (ok, label) if(!ok) => label }.mkString(", ")
              ("Not applied", Some(why))
          }
        case _ : ErrorLine => ("N/A",None)
    }

    val (complianceClass,compliancePercent) = {
      line match {
        case line : OKLine => buildComplianceChart(line.compliance, line.rule, nodes)
        case _ => ("noCompliance","N/A")
      }
    }

    val (applying,checkBoxClick) = {
      directiveApplication match {
        case Some(directiveApplication) =>
          def check(value : Boolean) : JsCmd= {
            directiveApplication.checkRule(line.rule.id, value) match {
              case DirectiveApplicationResult(rules,completeCategories,indeterminate) =>
              JsRaw(s"""
                ${completeCategories.map(c => s"""$$('#${c.value}Checkbox').prop("indeterminate",false); """).mkString("\n")}
                ${completeCategories.map(c => s"""$$('#${c.value}Checkbox').prop("checked",${value}); """).mkString("\n")}
                ${indeterminate.map(c => s"""$$('#${c.value}Checkbox').prop("indeterminate",true); """).mkString("\n")}
              """)
            }
          }
          val isApplying = line.rule.directiveIds.contains(directiveApplication.directive.id)
          val callBack = SHtml.ajaxCall(JsVar("checked"), bool => check (bool.toBoolean))
          (isApplying,Some(callBack))
        case None => (false,None)
      }
    }

    val cssClass = {
      val disabled = if (line.rule.isEnabled) {
        ""
      } else {
        "disabledRule"
      }

      val error = line match {
        case _:ErrorLine => " error"
        case _ => ""
      }

      s"tooltipabletr ${disabled} ${error}"
    }

    val callback = detailsCallbackLink .map(cb => SHtml.ajaxCall(JsVar("action"), (s: String) => cb(line.rule,s)))

    // reason should be treated apart so it can be empty
    val reason =  reasons.map(r => ( "reasons" -> Str(r))).toSeq

    JsObj(
        ( "name" -> line.rule.name )
      , ( "id" -> line.rule.id.value )
      , ( "description" -> line.rule.shortDescription )
      , ( "applying" ->  applying )
      , ( "category" -> categoryService.shortFqdn(line.rule.categoryId).getOrElse("Error") )
      , ( "status" -> status )
      , ( "compliance" -> compliancePercent )
      , ( "complianceClass" -> complianceClass )
      , ( "trClass" -> cssClass )
      , ( "callback" -> callback.map(cb => AnonFunc("action",cb)).getOrElse(JsNull))
      , ( "checkboxCallback" -> checkBoxClick.map(cb => AnonFunc("checked",cb)).getOrElse(JsNull))
      , ( "actions" -> "")
    ).+* (
      // Add reason, so it can be empty
      JsObj(reason:_*)
    )


  }

  private[this] def computeCompliances(rules: Set[Rule]) : Map[RuleId, Box[Option[ComplianceLevel]]] = {
    reportingService.findImmediateReportsByRules(rules.map(_.id)).map { case (ruleId, entry) =>
      (ruleId,
          entry match {
            case e:EmptyBox => e
            case Full(None) => Full(Some(Applying)) // when we have a rule but nothing in the database, it means that it is currently being deployed
            case Full(Some(x)) if (x.directivesOnNodesExpectedReports.size==0) => Full(None)
            case Full(Some(x)) if x.getNodeStatus().exists(x => x.nodeReportType == PendingReportType ) => Full(Some(Applying))
            case Full(Some(x)) =>  Full(Some(new Compliance((100 * x.getNodeStatus().filter(x => (x.nodeReportType == SuccessReportType || x.nodeReportType == NotApplicableReportType)).size) / x.getNodeStatus().size)))
          }
      )
    }
  }

  private[this] def buildComplianceChart(level:Option[ComplianceLevel], rule: Rule, allNodes: Set[NodeInfo]) : (String,String) = {
      level match {
        case None => ("noCompliance","N/A")
        case Some(Applying) => ("applyingCompliance","Applying")
        case Some(NoAnswer) => ("noCompliance","No answer")
        case Some(Compliance(percent)) =>
          val complianceClass =  if (percent <= 10 ) "redCompliance"
            else if(percent >= 90) "greenCompliance"
              else "orangeCompliance"
          val text = percent.toString + "%"

          (complianceClass,text)
      }
  }
}

sealed trait ComplianceLevel
case object Applying extends ComplianceLevel
case object NoAnswer extends ComplianceLevel
case class Compliance(val percent:Int) extends ComplianceLevel with HashcodeCaching

