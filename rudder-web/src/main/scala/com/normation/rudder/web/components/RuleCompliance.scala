package com.normation.rudder.web.components

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
import com.normation.rudder.web.model.WBTextField
import com.normation.rudder.web.model.WBTextAreaField
import com.normation.rudder.web.model.WBSelectField

object RuleCompliance {
  private def details =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentRuleEditForm" :: Nil)
    } yield {
      chooseTemplate("component", "details", xml)
    }) openOr Nil
}

class RuleCompliance (rule : Rule) extends Loggable {

  private[this] val reportingService = RudderConfig.reportingService
  private[this] val categoryHierarchyDisplayer = RudderConfig.categoryHierarchyDisplayer
  private[this] val roCategoryRepository = RudderConfig.roRuleCategoryRepository
  private[this] val categoryService      = RudderConfig.ruleCategoryService

  val popup = new RuleCompliancePopup(rule)

    /////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Edit form ///////////////////////////////
  /////////////////////////////////////////////////////////////////////////

  ///////////// fields for Rule settings ///////////////////

  private[this] val crName = new WBTextField("Name", rule.name) {
    override def setFilter = notNull _ :: trim _ :: Nil
    override def className = "twoCol"
    override def validations =
      valMinLen(3, "The name must have at least 3 characters") _ :: Nil
  }

  private[this] val crShortDescription = {
    new WBTextField("Short description", rule.shortDescription) {
      override def className = "twoCol"
      override def setFilter = notNull _ :: trim _ :: Nil
      override val maxLen = 255
      override def validations =  Nil
    }
  }

  private[this] val crLongDescription = {
    new WBTextAreaField("Description", rule.longDescription.toString) {
      override def setFilter = notNull _ :: trim _ :: Nil
      override def className = "twoCol"
    }
  }

  private[this] val category =
    new WBSelectField(
        "Rule category"
      , categoryHierarchyDisplayer.getRuleCategoryHierarchy(roCategoryRepository.getRootCategory.get, None).map { case (id, name) => (id.value -> name)}
      , rule.categoryId.value
    ) {
    override def className = "twoCol"
  }

  def  display(directiveLib: FullActiveTechniqueCategory, allNodeInfos: Map[NodeId, NodeInfo]) : NodeSeq = {

    (
      "#details *" #> { (n:NodeSeq) => SHtml.ajaxForm(n) } andThen
      "#nameField" #>    <div>{crName.displayNameHtml.getOrElse("Could not fetch rule name")} {rule.name} </div> &
      "#categoryField" #> <div> {category.displayNameHtml.getOrElse("Could not fetch rule category")} {categoryService.shortFqdn(rule.categoryId).getOrElse("Could not fetch rule category")}</div> &
      "#rudderID" #> {rule.id.value.toUpperCase} &
      "#shortDescriptionField" #>  <div>{crShortDescription.displayNameHtml.getOrElse("Could not fetch short description")} {rule.shortDescription}</div> &
      "#longDescriptionField" #>  <div>{crLongDescription.displayNameHtml.getOrElse("Could not fetch description")} {rule.longDescription}</div> &
      "#compliancedetails" #> showCompliance(directiveLib, allNodeInfos)
    )(RuleCompliance.details)
  }

  /*
   * For each table : the subtable is contained in td : details
   * when + is clicked: it gets the content of td details then process it has a datatable
   */
  def showCompliance(directiveLib: FullActiveTechniqueCategory, allNodeInfos: Map[NodeId, NodeInfo]) : NodeSeq = {

    def showReportDetail(batch : Box[Option[ExecutionBatch]], directiveLib: FullActiveTechniqueCategory) : NodeSeq = {
      batch match {
        case e: EmptyBox => <div class="error">Error while fetching report information</div>
        case Full(None) => NodeSeq.Empty
        case Full(Some(reports)) =>
          val directivesreport=reports.getRuleStatus().filter(dir => rule.directiveIds.contains(dir.directiveId))
          val tooltipid = Helpers.nextFuncName
          val directives = for {
            directiveStatus <- directivesreport
            (fullActiveTechnique, directive) <- directiveLib.allDirectives.get(directiveStatus.directiveId)
          } yield {
            val severity = ReportType.getSeverityFromStatus(directiveStatus.directiveReportType)
            val status = getDisplayStatusFromSeverity(directiveStatus.directiveReportType)
            val components= showComponentsReports(directiveStatus.components)
            val ajaxCall = SHtml.ajaxCall(JsNull, (s) => popup.showPopup(directiveStatus, directiveLib, allNodeInfos))
            val callback = AnonFunc("",ajaxCall)
            JsObj(
                ( "directive" -> directive.name )
              , ( "compliance" -> buildComplianceChart(directiveStatus) )
              , ( "status" -> status )
              , ( "statusClass" -> severity )
              , ( "id" -> directive.id.value )
              , ( "details" -> components )
              , ( "callback" -> callback )
            )
          }
          val data = JsArray(directives.toList)
          <table id="reportsGrid" cellspacing="0">  </table>++
          Script(JsRaw(s"""createDirectiveTable("reportsGrid",${data.toJsCmd},1); """))
 /*
                  case Some((fullActiveTechnique, directive))  =>
 {que.techniques.get(directive.techniqueVersion).map(_.name).getOrElse("Unknown technique")
                    val techversion = directive.techniqueVersion;
                    val tooltipid = Helpers.nextFuncName

                      "#directive *" #>{
                        <span>
                          <b>{directive.name}</b>
                          <span class="tooltipable" tooltipid={tooltipid} title="">
                            <img   src="/images/icInfo.png" style="padding-left:4px"/>
                          </span>
                          { val xml = <img   src="/images/icPen.png" style="padding-left:4px" class="noexpand"/>
                            SHtml.a( {()=> RedirectTo("""/secure/configurationManager/directiveManagement#{"directiveId":"%s"}""".format(directive.id.value))},xml,("style","padding-left:4px"),("class","noexpand"))
                          }
                          <div class="tooltipContent" id={tooltipid}>
                            Directive <b>{directive.name}</b> is based on technique
                            <b>{tech}</b> (version {techversion})
                          </div>
                        </span> }&
                  */
      }
    }

    /*
     * Display component details of a directive and add its compoenent value details if needed
     */
    def showComponentsReports(components : Seq[ComponentRuleStatusReport]) = {

        val id = Helpers.nextFuncName



              //components.flatMap { component =>
  /*
            if (component.componentValues.forall( x => x.componentValue =="None")) {
              // only None, we won't show the details, we don't need the plus and that td should not be clickable
              ("* [class+]" #> "noexpand").apply(componentDetails)
            } else {
              // standard  display that can be expanded
              val tooltipid = Helpers.nextFuncName
              val value = showComponentValueReport(component.componentValues,worstseverity)
              ( "#details *" #>  value &
                "tr [class+]" #> "cursor" &
                "#component [class+]" #>  "listopen"
              ) (componentDetails )
            }
      )  }*/
        val Values = for {
          component <- components
          severity = ReportType.getSeverityFromStatus(component.componentReportType)
          status = getDisplayStatusFromSeverity(severity)
          value = showComponentValueReport(component.componentValues)
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
          , ( "callback" -> callback)
            )
        }
         JsArray(Values.toList)
    }

    /*
     * Display component value details
     */
  def showComponentValueReport(values : Seq[ComponentValueRuleStatusReport]) = {
          val id = Helpers.nextFuncName
        // we need to group all the ComponentValueRuleStatusReports by unexpandedComponentValue if any, or by component value
        // and agregate them together
        val reportsByComponents = values.groupBy { entry => entry.unexpandedComponentValue.getOrElse(entry.componentValue)}
        val Values = for {
          (key,entries) <- reportsByComponents
          severity = ReportType.getWorseType(entries.map(_.cptValueReportType))

          value =
            ComponentValueRuleStatusReport(
                entries.head.directiveid // can't fail because we are in a groupBy
              , entries.head.component  // can't fail because we are in a groupBy
              , key
              , None // TODO : is it what we want ??
              , severity
              , entries.flatMap(_.reports)
            )
        } yield {

        val ajaxCall = reportsByComponents.get(value.componentValue) match {
          case Some(reports) => SHtml.ajaxCall(JsNull, (s) => popup.showPopup(value, reports, directiveLib, allNodeInfos))
          case None => SHtml.ajaxCall(JsNull, (s) => popup.showPopup(value, directiveLib, allNodeInfos))
        }
          val callback = AnonFunc("",ajaxCall)
          val severity = ReportType.getSeverityFromStatus(value.cptValueReportType)
          val status = getDisplayStatusFromSeverity(severity)

        JsObj(
            ( "value" -> value.componentValue )
          , ( "compliance" -> buildComplianceChartForComponent(value) )
          , ( "status" -> status )
          , ( "statusClass" -> severity )
          , ( "callback" -> callback)
            )
        }
        val data = JsArray(Values.toList)

    <table id={id} cellspacing="0" style="display:none" class="noMarginGrid tablewidth "></table> ++
    Script(JsRaw(s"""createComponentValueTable("${id}",${data.toJsCmd}); """))

    data
    }


    def buildComplianceChart(rulestatusreport:RuleStatusReport) = {
      rulestatusreport.computeCompliance match {
        case Some(percent) => s"${percent}%"
        case None => "Not Applied"
      }
    }

    def buildComplianceChartForComponent(
      valueStatusReport: ComponentValueRuleStatusReport
    ) : String = {
      valueStatusReport.computeCompliance match {
        case Some(percent) =>  s"${percent}%"
        case None => "Not Applied"
      }
    }

    val batch = reportingService.findImmediateReportsByRule(rule.id)

    <div>
    <hr class="spacer" />
        {showReportDetail(batch, directiveLib)}
    </div>++ Script( OnLoad( After( TimeSpan(100), JsRaw("""createTooltip();"""))))
  }

  def getDisplayStatusFromSeverity(severity: String) : String = {
    S.?(s"reports.severity.${severity}")
  }

}