/*
*************************************************************************************
* Copyright 2014 Normation SAS
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

package com.normation.rudder.web.services

import com.normation.rudder.domain.policies._
import com.normation.rudder.repository.FullActiveTechniqueCategory
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.nodes.NodeInfo
import scala.xml._
import net.liftweb.http._
import net.liftweb.common._
import com.normation.rudder.domain.reports.bean._
import net.liftweb.util.Helpers._
import net.liftweb.util.Helpers
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd
import bootstrap.liftweb.RudderConfig
import com.normation.rudder.web.components.popup.RuleCompliancePopup

case class ComplianceData(
    rule : Rule
  , directiveLib : FullActiveTechniqueCategory
  , allNodeInfos : Map[NodeId, NodeInfo]
) {

  val popup = new RuleCompliancePopup(rule)

  def getDirectivesComplianceDetails (
      directivesReport: Seq[DirectiveRuleStatusReport]
  ) = {
    val directivesComplianceData = for {
      directiveStatus <- directivesReport
      (fullActiveTechnique, directive) <- directiveLib.allDirectives.get(directiveStatus.directiveId)
      techniqueName    = fullActiveTechnique.techniques.get(directive.techniqueVersion).map(_.name).getOrElse("Unknown technique")
      techniqueVersion = directive.techniqueVersion;
      severity   = ReportType.getSeverityFromStatus(directiveStatus.directiveReportType)
      status     = getDisplayStatusFromSeverity(directiveStatus.directiveReportType)
      components = getComponentsComplianceDetails (directiveStatus.components)
    } yield {
      val ajaxCall = SHtml.ajaxCall(JsNull, (s) => popup.showPopup(directiveStatus, directiveLib, allNodeInfos))
      val callback = AnonFunc("",ajaxCall)
      JsObj(
          ( "directive" -> directive.name )
        , ( "compliance" -> buildComplianceChart(directiveStatus) )
        , ( "techniqueName" -> techniqueName )
        , ( "techniqueVersion" -> techniqueVersion.toString )
        , ( "status" -> status )
        , ( "statusClass" -> severity )
        , ( "id" -> directive.id.value )
        , ( "details" -> components )
        , ( "callback" -> callback )
      )
    }

    JsArray(directivesComplianceData.toList)
  }

  /*
   * Get compliance details of Components of a Directive
   */
  def getComponentsComplianceDetails (
      components : Seq[ComponentRuleStatusReport]
  ) = {

    val componentsComplianceData = for {
      component <- components
      severity  = ReportType.getSeverityFromStatus(component.componentReportType)
      id = Helpers.nextFuncName
      status    = getDisplayStatusFromSeverity(severity)
      value     = getValuesComplianceDetails (component.componentValues)
      // do not display details of the components if there is no value to display (all equals to None)
      noExpand  = component.componentValues.forall( x => x.componentValue =="None")
    } yield {
      val ajaxCall = SHtml.ajaxCall(JsNull, (s) => popup.showPopup(component, directiveLib, allNodeInfos))
      val callback = AnonFunc("",ajaxCall)
        JsObj(
            ( "component" -> component.component )
          , ( "id" -> id )
          , ( "compliance" -> buildComplianceChart(component) )
          , ( "status" -> status )
          , ( "statusClass" -> severity )
          , ( "details" -> value )
          , ( "noExpand" -> noExpand )
          , ( "callback" -> callback )
        )
      }
      JsArray(componentsComplianceData.toList)
    }


  /*
   * Get compliance details of Value of a Component
   */
  def getValuesComplianceDetails (
      values : Seq[ComponentValueRuleStatusReport]
  ) = {
    val valuesComplianceData = for {
      // we need to group all reports by their key ( if there is several reports with the same key)
      (key,entries) <- values.groupBy { entry => entry.key }
      value =
        ComponentValueRuleStatusReport (
            entries.head.directiveid // can't fail because we are in a groupBy
          , entries.head.component  // can't fail because we are in a groupBy
          , key
          , None // TODO : is it what we want ??
          , ReportType.getWorseType(entries.map(_.cptValueReportType))
          , entries.flatMap(_.reports)
        )
      severity = ReportType.getSeverityFromStatus(value.cptValueReportType)
      status = getDisplayStatusFromSeverity(severity)
    } yield {
      val ajaxCall = SHtml.ajaxCall(JsNull, (s) => popup.showPopup(value, entries, directiveLib, allNodeInfos))
      val callback = AnonFunc("",ajaxCall)
      JsObj(
          ( "value" -> value.componentValue )
        , ( "compliance" -> buildComplianceChart(value) )
        , ( "status" -> status )
        , ( "statusClass" -> severity )
        , ( "callback" -> callback)
      )
    }

    JsArray(valuesComplianceData.toList)
  }

  // Helpers function

  def buildComplianceChart(rulestatusreport:RuleStatusReport) = {
    rulestatusreport.computeCompliance match {
      case Some(percent) => s"${percent}%"
      case None => "Not Applied"
    }
  }

  def getDisplayStatusFromSeverity(severity: String) : String = {
    S.?(s"reports.severity.${severity}")
  }

}