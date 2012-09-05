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

import com.normation.rudder.domain.policies._
import com.normation.rudder.services.policies.RuleTargetService
import com.normation.rudder.batch.{AsyncDeploymentAgent,AutomaticStartDeployment}
import com.normation.rudder.repository._
import com.normation.rudder.domain.policies._
import net.liftweb.http.js._
import JsCmds._
import com.normation.rudder.domain.nodes.{NodeGroup, NodeGroupCategoryId, NodeGroupCategory, NodeGroupId}
import com.normation.cfclerk.services.TechniqueRepository
import com.normation.cfclerk.domain.Technique
import JE._
import net.liftweb.common._
import net.liftweb.http._
import scala.xml._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import com.normation.rudder.web.model._
import bootstrap.liftweb.LiftSpringApplicationContext.inject
import com.normation.utils.StringUuidGenerator
import com.normation.rudder.domain.eventlog.RudderEventActor
import com.normation.plugins.{SpringExtendableSnippet,SnippetExtensionKey}
import net.liftweb.json._
import com.normation.rudder.domain.eventlog.{
  DeleteRule,
  ModifyRule
}
import com.normation.rudder.web.services.JsTreeUtilService
import com.normation.rudder.web.services.UserPropertyService
import org.joda.time.DateTime
import com.normation.rudder.authorization._
import com.normation.rudder.domain.reports.bean._
import com.normation.rudder.services.reports.ReportingService
import com.normation.rudder.services.nodes.NodeInfoService
import com.normation.inventory.domain.NodeId
import com.normation.exceptions.TechnicalException
import net.liftweb.http.SHtml.BasicElemAttr

object RuleEditForm {
  
  /**
   * This is part of component static initialization.
   * Any page which contains (or may contains after an ajax request)
   * that component have to add the result of that method in it.
   */
  def staticInit:NodeSeq = 
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentRuleEditForm" :: Nil)
    } yield {
      chooseTemplate("component", "staticInit", xml)
    }) openOr Nil
    
  private def body(tab :Int = 0) =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentRuleEditForm" :: Nil)
    } yield {
      chooseTemplate("component", "body", xml)
    }) openOr Nil

  private def crForm = 
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentRuleEditForm" :: Nil)
    } yield {
      chooseTemplate("component", "form", xml)
    }) openOr Nil
    
  private def details =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentRuleEditForm" :: Nil)
    } yield {
      chooseTemplate("component", "details", xml)
    }) openOr Nil

  private def popupRemoveForm = 
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentRuleEditForm" :: Nil)
    } yield {
      chooseTemplate("component", "popupRemoveForm", xml)
    }) openOr Nil

  private def popupDisactivateForm = 
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentRuleEditForm" :: Nil)
    } yield {
      chooseTemplate("component", "popupDisactivateForm", xml)
    }) openOr Nil
    
  val htmlId_groupTree = "groupTree"
  val htmlId_activeTechniquesTree = "userPiTree"
}

/**
 * The form that handles Rule edition
 * (not creation)
 * - update name, description, etc
 * - update parameters 
 *
 * It handle save itself (TODO: see how to interact with the component parent)
 *
 * Parameters can not be null. 
 * 
 * Injection should not be used in components
 * ( WHY ? I will try to see...)
 * 
 */
class RuleEditForm(
  htmlId_rule:String, //HTML id for the div around the form
  var rule:Rule, //the Rule to edit
  //JS to execute on form success (update UI parts)
  //there are call by name to have the context matching their execution when called
  onSuccessCallback : () => JsCmd = { () => Noop },
  onFailureCallback : () => JsCmd = { () => Noop }
) extends DispatchSnippet with SpringExtendableSnippet[RuleEditForm] with Loggable {
  import RuleEditForm._
  
  private[this] val htmlId_save = htmlId_rule + "Save"
  private[this] val htmlId_EditZone = "editRuleZone"
  private[this] val ruleRepository = inject[RuleRepository]
  private[this] val targetInfoService = inject[RuleTargetService]
  private[this] val directiveRepository = inject[DirectiveRepository]
  private[this] val activeTechniqueCategoryRepository = inject[ActiveTechniqueCategoryRepository]
  private[this] val activeTechniqueRepository = inject[ActiveTechniqueRepository]
  private[this] val techniqueRepository = inject[TechniqueRepository]
  private[this] val uuidGen = inject[StringUuidGenerator]
  private[this] val asyncDeploymentAgent = inject[AsyncDeploymentAgent]
    private[this] val reportingService = inject[ReportingService]
  private[this] val nodeInfoService = inject[NodeInfoService]
  private[this] var crCurrentStatusIsActivated = rule.isEnabledStatus

  private[this] var selectedTargets = rule.targets
  private[this] var selectedDirectiveIds = rule.directiveIds

  private[this] val groupCategoryRepository = inject[NodeGroupCategoryRepository]
  private[this] val nodeGroupRepository = inject[NodeGroupRepository]

  private[this] val rootCategoryId = groupCategoryRepository.getRootCategory.id
  private[this] val treeUtilService = inject[JsTreeUtilService]
  private[this] val userPropertyService = inject[UserPropertyService]

  //////////////////////////// public methods ////////////////////////////
  val extendsAt = SnippetExtensionKey(classOf[RuleEditForm].getSimpleName)
  
  def mainDispatch = Map(
    "showForm" -> { _:NodeSeq => showForm() },
    "showEditForm" -> { _:NodeSeq => showForm(1) }
  )

  private[this] def showForm(tab :Int = 0) : NodeSeq = {
    val form = if (CurrentUser.checkRights(Read("rule"))) {
    ("#details" #> showRuleDetails())(
    if (CurrentUser.checkRights(Edit("rule"))) { (

      "#editForm" #> showCrForm() &
      "#removeActionDialog" #> showRemovePopupForm() &
      "#disactivateActionDialog" #> showDisactivatePopupForm()
      )(body (tab))
    }
    else (
      "#editForm" #>  <div>You have no rights to see rules details, please contact your administrator</div>
      ) (body()))
    }
    else {
      <div>You have no rights to see rules details, please contact your administrator</div>
    } 
    form ++  Script(OnLoad(JsRaw("""$( "#editRuleZone" ).tabs();
          $( "#editRuleZone" ).tabs('select', %s);""".format(tab)) )&
          JsRaw("""
              | $("#%s").bind( "tabsshow", function(event, ui) {
              | if(ui.panel.id== '%s') { %s; }
              | });
              """.stripMargin('|').format("editRuleZone",
            "ruleComplianceTab",
            SHtml.ajaxCall(JsRaw("'"+rule.id.value+"'"),(v:String) => Replace("details",showRuleDetails()))._2.toJsCmd
      )))
  }
  
  private[this] def showRemovePopupForm() : NodeSeq = {    
   (
       "#removeActionDialog *" #> { (n:NodeSeq) => SHtml.ajaxForm(n) } andThen
       "#dialogRemoveButton" #> { removeButton % ("id", "removeButton") } &
       ".reasonsFieldsetPopup" #> { crReasonsRemovePopup.map { f =>
         "#explanationMessage" #> <div>{userPropertyService.reasonsFieldExplanation}</div> &
         "#reasonsField" #> f.toForm_!
       } } &
       "#errorDisplay *" #> { updateAndDisplayNotifications(formTrackerRemovePopup) }
   )(popupRemoveForm) 
  }
  
  private[this] def showDisactivatePopupForm() : NodeSeq = {    
   (
      "#desactivateActionDialog *" #> { (n:NodeSeq) => SHtml.ajaxForm(n) } andThen
      "#dialogDisactivateButton" #> { disactivateButton % ("id", "disactivateButton") } &
      "#dialogDeactivateTitle" #> { if(crCurrentStatusIsActivated) "Disable" else "Enable" } &
      "#dialogDisactivateLabel" #> { if(crCurrentStatusIsActivated) "disable" else "enable" } &
      ".reasonsFieldsetPopup" #> { crReasonsDisactivatePopup.map { f =>
         "#explanationMessage" #> <div>{userPropertyService.reasonsFieldExplanation}</div> &
         "#reasonsField" #> f.toForm_!
      } } &
      "#errorDisplay *" #> { updateAndDisplayNotifications(formTrackerDisactivatePopup) }
   )(popupDisactivateForm) 
  }

  private[this] def  showRuleDetails() : NodeSeq = {
    val updatedrule = ruleRepository.get(rule.id)
    (
      "#details *" #> { (n:NodeSeq) => SHtml.ajaxForm(n) } andThen
      "#nameField" #>    <div>{crName.displayNameHtml.get} {updatedrule.map(_.name).openOr("could not fetch rule name")}</div> &
      "#shortDescriptionField" #>  <div>{crShortDescription.displayNameHtml.get} {updatedrule.map(_.shortDescription).openOr("could not fetch rule short descritption")}</div> &
      "#longDescriptionField" #>  <div>{crLongDescription.displayNameHtml.get} {updatedrule.map(_.longDescription).openOr("could not fetch rule long descritption")}</div> &
      "#compliancedetails" #> showCompliance(updatedrule.get)
    )(details) ++
    Script(JsRaw("""
        $(".unfoldable").click(function(event) {
          event.stopPropagation();
          if(!($(this).is("a"))){
            var togglerId = $(this).attr("toggler");
            var totoggle = $(this).siblings('#'+togglerId)
            var td = $(this).find("td.listclose")
            if (td.length > 0){
              totoggle.each( function() {
                if( $(this).find("td.listclose").length != 0)
                  $(this).click();
              } )
              $(this).find("td.listclose").removeClass("listclose").addClass("listopen");
            } else
              $(this).find("td.listopen").removeClass("listopen").addClass("listclose");
            totoggle.each( function() { $(this).toggle() } );
          }
        } );""")
    );
  }
 

  private[this] def showCrForm() : NodeSeq = {    
    (
      "#editForm *" #> { (n:NodeSeq) => SHtml.ajaxForm(n) } andThen
      ClearClearable &
      //activation button: show disactivate if activated
      "#disactivateButtonLabel" #> { if(crCurrentStatusIsActivated) "Disable" else "Enable" } &
      "#removeAction *" #> {
         SHtml.ajaxButton("Delete", () => createPopup("removeActionDialog",140,450),("type", "button"))
       } &
       "#desactivateAction *" #> {
         val status = crCurrentStatusIsActivated ? "Disable" | "Enable"
         SHtml.ajaxButton(   status, () => createPopup("desactivateActionDialog",100,450),("type", "button"))
       } &
      "#nameField" #> crName.toForm_! &
      "#shortDescriptionField" #> crShortDescription.toForm_! &
      "#longDescriptionField" #> crLongDescription.toForm_! &
      ".reasonsFieldset" #> { crReasons.map { f =>
        "#explanationMessage" #> <div>{userPropertyService.reasonsFieldExplanation}</div> &
        "#reasonsField" #> f.toForm_!
      } } &
      "#selectPiField" #> {
        <div id={htmlId_activeTechniquesTree}>
          <ul>{ activeTechniqueCategoryToJsTreeNode(
              activeTechniqueCategoryRepository.getActiveTechniqueLibrary
            ).toXml }
          </ul>
        </div> } &
      "#selectGroupField" #> { 
        <div id={htmlId_groupTree}>
          <ul>
            {nodeGroupCategoryToJsTreeNode(groupCategoryRepository.getRootCategory).toXml}
          </ul>
        </div> } &
      "#save" #> saveButton &
      "#notification *" #>  updateAndDisplayNotifications(formTracker) &
      "#editForm [id]" #> htmlId_rule
    )(crForm) ++ 
    Script(OnLoad(JsRaw("""
      correctButtons();
    """)))++ Script(
        //a function to update the list of currently selected PI in the tree
        //and put the json string of ids in the hidden field. 
        JsCrVar("updateSelectedPis", AnonFunc(JsRaw("""
          $('#selectedPis').val(JSON.stringify(
            $.jstree._reference('#%s').get_selected().map(function(){
              return this.id;
            }).get()));""".format(htmlId_activeTechniquesTree) 
        ))) &
        JsCrVar("updateSelectedTargets", AnonFunc(JsRaw("""
          $('#selectedTargets').val(JSON.stringify(
            $.jstree._reference('#%s').get_selected().map(function(){
              return this.id;
            }).get()));""".format(htmlId_groupTree) 
        ))) &
      OnLoad(
        //build jstree and
        //init bind callback to move
        JsRaw("buildGroupTree('#%1$s', %2$s, 'on');".format(
            htmlId_groupTree,
            serializeTargets(selectedTargets.toSeq)
        )) &
        //function to update list of PIs before submiting form
        JsRaw("buildRulePIdepTree('#%1$s', %2$s);".format(  
            htmlId_activeTechniquesTree,
            serializedirectiveIds(selectedDirectiveIds.toSeq)
        )) &
        After(TimeSpan(50), JsRaw("""createTooltip();"""))
      )
    )
  }
  
  /*
   * from a list of PI ids, get a string.
   * the format is a JSON array: [ "id1", "id2", ...] 
   */
  private[this] def serializedirectiveIds(ids:Seq[DirectiveId]) : String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    Serialization.write(ids.map( "jsTree-" + _.value ))
  }
  
  private[this] def serializeTargets(targets:Seq[RuleTarget]) : String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    Serialization.write(
        targets.map { target => 
          target match { 
            case GroupTarget(g) => "jsTree-" + g.value
            case _ => "jsTree-" + target.target
          }
        }
    )
  }
  
  /*
   * from a JSON array: [ "id1", "id2", ...], get the list of
   * Directive Ids. 
   * Never fails, but returned an empty list. 
   */
  private[this] def unserializedirectiveIds(ids:String) : Seq[DirectiveId] = {
    implicit val formats = DefaultFormats 
    parse(ids).extract[List[String]].map( x => DirectiveId(x.replace("jsTree-","")) )
  }
  
  private[this] def unserializeTargets(ids:String) : Seq[RuleTarget] = {
    implicit val formats = DefaultFormats 
    parse(ids).extract[List[String]].map{x => 
      GroupTarget(NodeGroupId(x.replace("jsTree-","")))
    }
  }
 
  
  def createPopup(name:String,height:Int,width:Int) :JsCmd = {
    JsRaw("""createPopup("%s",%s,%s);""".format(name,height,width))
  }
  
  ////////////// Callbacks //////////////
  
  
  private[this] def onSuccess() : JsCmd = {
    //MUST BE THIS WAY, because the parent may change some reference to JsNode
    //and so, our AJAX could be broken
    onSuccessCallback() & updateFormClientSide() & 
    //show success popup
    successPopup
  }
  
  private[this] def onFailure() : JsCmd = {
    onFailureCallback() & 
    updateFormClientSide() & 
    JsRaw("""scrollToElement("notifications");""")
  }
  
  private[this] def onFailureRemovePopup() : JsCmd = {
    updateRemoveFormClientSide() & 
    onFailureCallback()
  }
  
  private[this] def onFailureDisablePopup() : JsCmd = {
    onFailureCallback() & 
    updateDisableFormClientSide()
  }
  
  ///////////// Remove ///////////// 
    
  private[this] def removeButton : Elem = {
    def removeCr() : JsCmd = {
      if(formTrackerRemovePopup.hasErrors) {
        onFailureRemovePopup
      } else {
        JsRaw("$.modal.close();") & 
        { 
          (for {
            save   <- ruleRepository.delete(rule.id, CurrentUser.getActor, 
                        crReasonsRemovePopup.map( _.is))
            deploy <- {
              asyncDeploymentAgent ! AutomaticStartDeployment(RudderEventActor)
              Full("Deployment request sent")
            }
          } yield {
            save 
          }) match {
            case Full(x) => 
              onSuccessCallback() & 
              SetHtml("details",
                <div id={"details"}>Rule successfully deleted</div>
              ) &
              SetHtml(htmlId_rule, 
                <div id={htmlId_rule}>Rule successfully deleted</div>
              ) & 
              //show success popup
              successPopup 
            case Empty => //arg. 
              formTrackerRemovePopup.addFormError(
                  error("An error occurred while deleting the Rule"))
              onFailure()
            case Failure(m,_,_) =>
              formTrackerRemovePopup.addFormError(
                  error("An error occurred while saving the Rule: " + m))
              onFailure()
          }
        }
      }
    }
    
    SHtml.ajaxSubmit("Delete", removeCr _ )
  }
  
  
  ///////////// Activation / disactivation ///////////// 
  
  
  private[this] def disactivateButton : Elem = {
    def switchActivation(status:Boolean)() : JsCmd = {
      if(formTrackerDisactivatePopup.hasErrors) {
        onFailureDisablePopup
      } else {
        crCurrentStatusIsActivated = status
        JsRaw("$.modal.close();") & 
        saveAndDeployRule(rule.copy(isEnabledStatus = status), crReasonsDisactivatePopup.map(_.is))
      }
    }
    
    if(crCurrentStatusIsActivated) {
      SHtml.ajaxSubmit("Disable", switchActivation(false) _ )
    } else {
      SHtml.ajaxSubmit("Enable", switchActivation(true) _ )
    }
  }
  
  /*
   * Create the ajax save button
   */
  private[this] def saveButton : NodeSeq = { 
    // add an hidden field to hold the list of selected directives
    val save = SHtml.ajaxSubmit("Save", onSubmit _) % ("id" -> htmlId_save) 
    // update onclick to get the list of directives and groups in the hidden 
    // fields before submitting

    val newOnclick = "updateSelectedPis(); updateSelectedTargets(); " + 
      save.attributes.asAttrMap("onclick")

    SHtml.hidden( { ids => 
        selectedDirectiveIds = unserializedirectiveIds(ids).toSet
      }, serializedirectiveIds(selectedDirectiveIds.toSeq) 
    ) % ( "id" -> "selectedPis") ++ 
    SHtml.hidden( { targets => 
        selectedTargets = unserializeTargets(targets).toSet
      }, serializeTargets(selectedTargets.toSeq) 
    ) % ( "id" -> "selectedTargets") ++ 
    save % ( "onclick" -> newOnclick)
  }
  
  
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
      override def inputField = super.inputField  % 
        ("style" -> "height:10em")
    }
  }

  private[this] val crReasons = {
    import com.normation.rudder.web.services.ReasonBehavior._
    userPropertyService.reasonsFieldBehavior match {
      case Disabled => None
      case Mandatory => Some(buildReasonField(true, "subContainerReasonField"))
      case Optionnal => Some(buildReasonField(false, "subContainerReasonField"))
    }
  }
  
  private[this] val crReasonsDisactivatePopup = {
    import com.normation.rudder.web.services.ReasonBehavior._
    userPropertyService.reasonsFieldBehavior match {
      case Disabled => None
      case Mandatory => Some(buildReasonField(true, "subContainerReasonField"))
      case Optionnal => Some(buildReasonField(false, "subContainerReasonField"))
    }
  }
  
  private[this] val crReasonsRemovePopup = {
    import com.normation.rudder.web.services.ReasonBehavior._
    userPropertyService.reasonsFieldBehavior match {
      case Disabled => None
      case Mandatory => Some(buildReasonField(true, "subContainerReasonField"))
      case Optionnal => Some(buildReasonField(false, "subContainerReasonField"))
    }
  }
  
  def buildReasonField(mandatory:Boolean, containerClass:String = "twoCol") = {
    new WBTextAreaField("Message", "") {
      override def setFilter = notNull _ :: trim _ :: Nil
      override def inputField = super.inputField  % 
        ("style" -> "height:8em;")
      override def subContainerClassName = containerClass
      override def validations() = {
        if(mandatory){
          valMinLen(5, "The reasons must have at least 5 characters.") _ :: Nil
        } else {
          Nil
        }
      }
    }
  }

  private[this] def addCurrentNodeGroup(group : NodeGroup): Unit = {
    selectedTargets = selectedTargets + GroupTarget(group.id)
  }

  private[this] val formTracker = {
    val fields = List(crName, crShortDescription, crLongDescription) ++ 
      crReasons.toList
    new FormTracker(fields) 
  }
  
  private[this] val formTrackerRemovePopup = {
    new FormTracker(crReasonsRemovePopup.toList) 
  }
  
  private[this] val formTrackerDisactivatePopup = {
    new FormTracker(crReasonsDisactivatePopup.toList) 
  }
  
  private[this] def activateButtonOnChange() : JsCmd = {
    JsRaw("""activateButtonOnFormChange("%s", "%s");  """.format(htmlId_rule,htmlId_save) )
  }
  
  private[this] def updateFormClientSide() : JsCmd = {
    Replace(htmlId_EditZone, this.showForm(1) )
  }

  private[this] def updateRemoveFormClientSide() : JsCmd = {
    val jsDisplayRemoveDiv = JsRaw("""$("#removeActionDialog").removeClass('nodisplay')""")
    Replace("removeActionDialog", this.showRemovePopupForm()) & 
    jsDisplayRemoveDiv &
    initJs
  }
  
  private[this] def updateDisableFormClientSide() : JsCmd = {
    val jsDisplayDisableDiv = JsRaw("""$("#desactivateActionDialog").removeClass('nodisplay')""")
    Replace("desactivateActionDialog", this.showDisactivatePopupForm()) & 
    jsDisplayDisableDiv &
    initJs
  }
  
  def initJs : JsCmd = {
    JsRaw("correctButtons();")
  }
  
  private[this] def error(msg:String) = <span class="error">{msg}</span>
  
    
  private[this] def onSubmit() : JsCmd = {
    if(formTracker.hasErrors) {
      onFailure
    } else { //try to save the rule
      val newCr = rule.copy(
        name = crName.is,
        shortDescription = crShortDescription.is,
        longDescription = crLongDescription.is,
        targets = selectedTargets,
        directiveIds = selectedDirectiveIds,
        isEnabledStatus = crCurrentStatusIsActivated
      )
      saveAndDeployRule(newCr, crReasons.map(_.is))
    }
  }
  
  private[this] def saveAndDeployRule(rule:Rule, reason: Option[String]) : JsCmd = {
      (for {
        save <- ruleRepository.update(rule, CurrentUser.getActor, reason)
        deploy <- {
          asyncDeploymentAgent ! AutomaticStartDeployment(RudderEventActor)
          Full("Deployment request sent")
        }
      } yield {
        save 
      }) match {
        case Full(x) => 
          this.rule = rule;
          onSuccess
        case Empty => //arg. 
          formTracker.addFormError(error("An error occurred while saving the Rule"))
          onFailure
        case f:Failure =>
          formTracker.addFormError(error(f.messageChain))
          onFailure
      }      
  }
  
  private[this] def updateAndDisplayNotifications(formTracker : FormTracker) : NodeSeq = {
    
    val notifications = formTracker.formErrors
    formTracker.cleanErrors
   
    if(notifications.isEmpty) {
      NodeSeq.Empty
    }
    else {
      val html = 
        <div id="notifications" class="notify">
          <ul class="field_errors">{notifications.map( n => <li>{n}</li>) }</ul>
        </div>
      html
    }
  }
  
  ///////////// success pop-up ///////////////
  private[this] def successPopup : JsCmd = {
    JsRaw(""" callPopupWithTimeout(200, "successConfirmationDialog", 100, 350)     
    """)
  }
    
 /********************************************
  * Utilitary methods for JS
  ********************************************/

  /**
   * Transform a NodeGroupCategory into category JsTree node :
   * - contains:
   *   - other categories
   *   - groups
   * -
   */

  private def nodeGroupCategoryToJsTreeNode(category:NodeGroupCategory) : JsTreeNode = {
    new JsTreeNode {
      override def body = {
        val tooltipid = Helpers.nextFuncName
          <a href="#">
          <span class="treeGroupCategoryName tooltipable" tooltipid={tooltipid} 
            title={category.description}>
            {category.name}
          </span>
        </a>
          <div class="tooltipContent" id={tooltipid}>
          <h3>{category.name}</h3>
          <div>{category.description}</div>
        </div>
      }
  
      override def children = {
        category.children.flatMap(x => nodeGroupCategoryIdToJsTreeNode(x)) ++ 
        category.items.map(x => policyTargetInfoToJsTreeNode(x))
      }
        
      override val attrs =
        ( "rel" -> { if(category.id == rootCategoryId) "root-category" else "category" } ) ::
        ( "catId" -> category.id.value ) ::
        ( "class" -> "" ) ::
        Nil
    }
  }


  //fetch node group category id and transform it to a tree node
  private def nodeGroupCategoryIdToJsTreeNode(id:NodeGroupCategoryId) : Box[JsTreeNode] = {
    groupCategoryRepository.getGroupCategory(id) match {
      //remove sytem category
      case Full(category) => category.isSystem match {
        case true => Empty
        case false => Full(nodeGroupCategoryToJsTreeNode(category))
      }
      case e:EmptyBox =>
        val f = e ?~! "Error while fetching Technique category %s".format(id)
        logger.error(f.messageChain)
        f
    }
  }

  //fetch node group id and transform it to a tree node
  private def policyTargetInfoToJsTreeNode(targetInfo:RuleTargetInfo) : JsTreeNode = {
    targetInfo.target match {
      case GroupTarget(id) =>
        nodeGroupRepository.getNodeGroup(id) match {
          case Full(group) => nodeGroupToJsTreeNode(group)
          case _ => new JsTreeNode {
            override def body = <span class="error">Can not find node {id.value}</span>
            override def children = Nil
          }
        }
      case x => new JsTreeNode {
         override def body =  {
           val tooltipid = Helpers.nextFuncName
           <span class="treeGroupName tooltipable" tooltipid={tooltipid} >
             {targetInfo.name} 
             <span title={targetInfo.description} class="greyscala">
               (special)
             </span>
             <div class="tooltipContent" id={tooltipid}>
               <h3>{targetInfo.name}</h3>
               <div>{targetInfo.description}</div>
             </div>
           </span>
         }
                               
         override def children = Nil
         override val attrs = ( "rel" -> "special_target" ) :: Nil
      }
    }
  }


  /**
   * Transform a WBNodeGroup into a JsTree leaf.
   */
  private def nodeGroupToJsTreeNode(group : NodeGroup) : JsTreeNode = {
    new JsTreeNode {
      //ajax function that update the bottom
      def onClickNode() : JsCmd = {
        addCurrentNodeGroup(group)
        Noop
      }
  
      override def body = {
        val tooltipid = Helpers.nextFuncName
        SHtml.a(onClickNode _,
          <span class="treeGroupName tooltipable" tooltipid={tooltipid} 
          title={group.description}>
            {List(group.name,group.isDynamic?"dynamic"|"static").mkString(": ")}
          </span>
          <div class="tooltipContent" id={tooltipid}>
            <h3>{group.name}</h3>
            <div>{group.description}</div>
          </div>)    
      }
      
      override def children = Nil
      
      override val attrs =
        ( "rel" -> "group" ) ::
        ( "groupId" -> group.id.value ) ::
        ( "id" -> ("jsTree-" + group.id.value) ) ::
        Nil
    }
  }


  /**
   * Transform ActiveTechniqueCategory into category JsTree nodes in User Library:
   * - contains
   *   - other user categories
   *   - Active Techniques

   */
  private def activeTechniqueCategoryToJsTreeNode(category:ActiveTechniqueCategory) : JsTreeNode = {
    /*
     *converts activeTechniqueId into Option[(JsTreeNode, Option[Technique])]
     *returns some(something) if the technique has some derivated directives, else returns none
     * */
    def activeTechniqueIdToJsTreeNode(id : ActiveTechniqueId) : Option[(JsTreeNode, Option[Technique])] = { 
      
      def activeTechniqueToJsTreeNode(activeTechnique : ActiveTechnique, technique:Technique) : JsTreeNode = {
        
        //check Directive existence and transform it to a tree node
        def directiveIdToJsTreeNode(directiveId : DirectiveId) : (JsTreeNode,Option[Directive]) = {
          
          directiveRepository.getDirective(directiveId) match {
            case Full(directive) =>  (
              new JsTreeNode {
                override def body = {
                  val tooltipid = Helpers.nextFuncName
                  <a href="#">
                    <span class="treeDirective tooltipable" tooltipid={tooltipid} 
                      title={directive.shortDescription}>
                      {directive.name}
                    </span>
                    <div class="tooltipContent" id={tooltipid}>
                      <h3>{directive.name}</h3>
                      <div>{directive.shortDescription}</div>
                    </div>
                  </a>
                }
                override def children = Nil
                override val attrs = {
                  ( "rel" -> "directive") :: 
                  ( "id" -> ("jsTree-" + directive.id.value)) :: 
                  ( if(!directive.isEnabled) 
                      ("class" -> "disableTreeNode") :: Nil 
                    else Nil
                  )
               }
              },
              Some(directive)
            )
            case x =>
              logger.error("Error while fetching node %s: %s".format(directiveId, x.toString))
              (new JsTreeNode {
                override def body = 
                  <span class="error">Can not find node {directiveId.value}</span>
                override def children = Nil
              }, 
              None)
          }
        }
        
        new JsTreeNode {      
          override val attrs = {
            ( "rel" -> "template") :: Nil ::: 
            ( if(!activeTechnique.isEnabled) 
                ("class" -> "disableTreeNode") :: Nil 
              else Nil 
            )
          }
          override def body = {
            val tooltipid = Helpers.nextFuncName            
              <a href="#">
                <span class="treeActiveTechniqueName tooltipable" 
                  tooltipid={tooltipid} title={technique.description}>
                  {technique.name}
                </span>
                <div class="tooltipContent" id={tooltipid}>
                <h3>{technique.name}</h3>
                <div>{technique.description}</div>
                </div>
              </a>
          }
      
          override def children = 
            activeTechnique.directives
              .map(x => directiveIdToJsTreeNode(x)).toList
              .sortWith { 
                case ( (_, None) , _  ) => true
                case (  _ ,  (_, None)) => false
                case ( (node1, Some(pi1)), (node2, Some(pi2)) ) => 
                  treeUtilService.sortPi(pi1,pi2)
              }
              .map { case (node, _) => node }
        }
      }
      
      activeTechniqueRepository.getActiveTechnique(id) match {
        case Full(activeTechnique) => 
          techniqueRepository.getLastTechniqueByName(activeTechnique.techniqueName) match {
            case Some(refPt) if activeTechnique.directives.size>0 => 
              Some( activeTechniqueToJsTreeNode(activeTechnique,refPt), Some(refPt))
            case Some(refPt) if activeTechnique.directives.size==0 => None
            case None => 
              Some(new JsTreeNode {
                override def body = 
                  <span class="error">Can not find node {activeTechnique.techniqueName}</span>
                override def children = Nil
              }, None)
          }

        case x =>
          logger.error("Error while fetching node %s: %s".format(id, x.toString))
          Some(new JsTreeNode {
            override def body = <span class="error">Can not find node {id.value}</span>
            override def children = Nil
          }, None)
        }
    }

    
    //chech Active Technique category id and transform it to a tree node
    def activeTechniqueCategoryIdToJsTreeNode(id:ActiveTechniqueCategoryId) : 
      Box[(JsTreeNode,ActiveTechniqueCategory)] = {
      
      activeTechniqueCategoryRepository.getActiveTechniqueCategory(id) match {
        //remove sytem category
        case Full(cat) => cat.isSystem match {
          case true => Empty
          case false => Full((activeTechniqueCategoryToJsTreeNode(cat),cat))
        }
        case e:EmptyBox =>
          val f = e ?~! "Error while fetching for Technique category %s".format(id)
          logger.error(f.messageChain)
          f
      }
    }

    new JsTreeNode {
      override def body = {
        val tooltipid = Helpers.nextFuncName
        <a href="#">
          <span class="treeActiveTechniqueCategoryName tooltipable" 
              tooltipid={tooltipid} title={category.description}>
            {Text(category.name)}
          </span>
          <div class="tooltipContent" id={tooltipid}>
            <h3>{category.name}</h3>
            <div>{category.description}</div>
          </div>
        </a>
      }
      override def children = {
        /*
         * sortedActiveTechnique contains only techniques that have directives
         */
        val sortedActiveTechnique = {
          category.items
            .map(x => activeTechniqueIdToJsTreeNode(x)).toList.flatten
            .sortWith {
              case ( (_, None) , _ ) => true
              case ( _ , (_, None) ) => false
              case ( (node1, Some(refPt1)) , (node2, Some(refPt2)) ) => 
                treeUtilService.sortPt(refPt1,refPt2)
            }
            .map { case (node,_) => node }
        }
      
        val sortedCat = {
          category.children
            .filter { categoryId => 
              activeTechniqueCategoryRepository.containsDirective(categoryId)
            }
            .flatMap(x => activeTechniqueCategoryIdToJsTreeNode(x))
            .toList
            .sortWith { case ( (node1, cat1) , (node2, cat2) ) => 
              treeUtilService.sortActiveTechniqueCategory(cat1,cat2)
            }
            .map { case (node,_) => node }
        }
                              
        val res = sortedActiveTechnique ++ sortedCat
        res
      }
      override val attrs = ( "rel" -> "category") :: Nil
    }
  }

  //////////////// Compliance ////////////////

  private[this] def showCompliance(rule: Rule) : NodeSeq = {

    def showReportDetail(batch : Option[ExecutionBatch]) : NodeSeq = {
      batch match {
            case None => NodeSeq.Empty
            case Some(reports) =>
    ( "#reportsGrid [class+]" #> "tablewidth fixedlayout reportTable" &
      "#reportLine" #> {
              reports.getRuleStatus().filter(dir => rule.directiveIds.contains(dir.directiveId)).flatMap { directiveStatus =>
                    directiveRepository.getDirective(directiveStatus.directiveId) match {
                    case Full(directive)  => {
                      val tech = directiveRepository.getActiveTechnique(directive.id).map(act => techniqueRepository.getLastTechniqueByName(act.techniqueName).map(_.name).getOrElse("Unknown technique")).getOrElse("Unknown technique")
                      val techversion = directive.techniqueVersion;
                      val tooltipid = Helpers.nextFuncName
                      val xml:NodeSeq = (
                              "#directive [class+]" #> "listopen" &
                              "#directive *" #>
                                <span>{directive.name}</span> &
                              "#technique *" #> <span>{"%s (%s)".format(tech,techversion)}</span> &
                              "#severity *" #> buildComplianceChart(directiveStatus) &
                              ".unfoldable [class+]" #> ReportType.getSeverityFromStatus(directiveStatus.directiveReportType).replaceAll(" ", "") &
                              ".unfoldable [toggler]" #> tooltipid
                       )(reportsLineXml)
                       xml ++ showComponentsReports(directiveStatus.components,tooltipid)
                     }
                     case x:EmptyBox =>
                     logger.error( (x?~! "An error occured when trying to load directive %s".format(directiveStatus.directiveId.value)),x)
                     <div class="error">Node with ID "{directiveStatus.directiveId.value}" is invalid</div>
                   }
              } }
    
    )(reportsGridXml)}
    }

  def showComponentsReports(components : Seq[ComponentRuleStatusReport], id:String) : NodeSeq = {
    components.flatMap { component => val severity = ReportType.getSeverityFromStatus(component.componentReportType).replaceAll(" ", "")
      ("#componentLine [class+]" #> severity &
       "#componentLine [id]" #> id &
       "td [class+]" #> "detailReport" &
       "#severity *" #>  buildComplianceChart(component) ) (
      component.componentValues.forall( x => x.componentValue =="None") match {
        case true => // only None, we won't show the details
          ( "#component *" #> SHtml.a({() => showPopup(component)}, Text(component.component)) &
            "#component [class+]" #> "firstTd"
          )(componentDetails)
        case false => // standard  display that can be expanded
          val tooltipid = Helpers.nextFuncName
           ( "#component *" #> <span>{component.component}</span> &
             "#component [class+]" #> "listopen firstTd" &
             ".unfoldable [toggler]" #> tooltipid
           )(componentDetails) ++ showComponentValueReport(component.componentValues,tooltipid)
      })
    }
  }
  
  def showComponentValueReport(values : Seq[ComponentValueRuleStatusReport],id:String) : NodeSeq = {
    values.flatMap { value => val severity = ReportType.getSeverityFromStatus(value.cptValueReportType).replaceAll(" ", "")
           (  "#valueLine [class+]" #> severity &
              "#valueLine [id]" #> id &
              "td [class+]" #> "detailReport" &
              "#componentValue [class+]" #> "firstTd" &
              "#componentValue *" #>  SHtml.a({() => showPopup(value)}, Text(value.componentValue)) &
              "#keySeverity *" #> buildComplianceChart(value)
           )(componentValueDetails)
    }
  }

  def reportsGridXml : NodeSeq = {
    <table id="reportsGrid" cellspacing="0">
      <thead>
        <tr class="head tablewidth">
          <th width="12%">Directive</th>
          <th width="12%">Component</th>
          <th width="35%">Value</th>
          <th width="28%">Technique</th>
          <th width="13%">Compliance<span/></th>
        </tr>
      </thead>
      <tbody>
        <div id="reportLine"/>
      </tbody>
    </table>
  }

  def reportsLineXml : NodeSeq = {
    <tr class="unfoldable">
      <td id="directive" colspan="3"></td>
      <td name="technique" ><div id="technique"/></td>
      <td name="severity"><div id="severity"/></td>
     </tr>
  }

  def componentDetails : NodeSeq = {
   <tr id="componentLine" class="detailedReportLine unfoldable severity" style="display:none">
      <td class="emptyTd"/>
      <td id="component" colspan="3"></td>
      <td name="severity"><div id="severity"/></td>
   </tr>

  }

  def componentValueDetails : NodeSeq = {
  <tr id="valueLine"  class="detailedReportLine severityClass severity " style="display:none">
      <td class="emptyTd" colspan="2"/>
      <td id="componentValue" colspan="2">></td>
      <td name="keySeverity"><div id="keySeverity"/></td>
  </tr>
  }

  
 def buildComplianceChart(rulestatusreport:RuleStatusReport) : NodeSeq = {
    rulestatusreport.computeCompliance match {
      case Some(percent) =>  {
        val text = Text(percent.toString + "%")
        val attr = BasicElemAttr("class","unfoldable")
        SHtml.a({() => showPopup(rulestatusreport)}, text,attr)
      }
      case None => Text("Not Applied")
    }
  }

  val batch = reportingService.findImmediateReportsByRule(rule.id)

  <div>
  <hr class="spacer" />
        {showReportDetail(batch)}
  </div>
  }

  ///////////////// Compliance detail popup/////////////////////////

  private[this] def createPopup(directivebynode: RuleStatusReport) : NodeSeq = {
      
   def templatePath = List("templates-hidden", "reports_grid")
   def template() =  Templates(templatePath) match {
     case Empty | Failure(_,_,_) =>
     throw new TechnicalException("Template for report grid not found. I was looking for %s.html".format(templatePath.mkString("/")))
     case Full(n) => n
   }
   def reportTemplate = chooseTemplate("reports", "report", template)
    
    
   
  def showNodeReports(nodeReports : Seq[(NodeId,ReportType,List[String])]) : NodeSeq= {
     val nodes = nodeReports.map(_._1).distinct
     val nodeStatuses = nodes.map(node => (node,ReportType.getWorseType(nodeReports.filter(_._1==node).map(stat => stat._2)),nodeReports.filter(_._1==node).flatMap(stat => stat._3).toList))
     nodeStatuses.toList match {
     case Nil => NodeSeq.Empty
     case nodeStatus :: rest =>
     val nodeReport = nodeInfoService.getNodeInfo(nodeStatus._1) match {
     case Full(nodeInfo) => {
       val tooltipid = Helpers.nextFuncName
               ("#node *" #>
               <a class="unfoldable" href={"""secure/nodeManager/searchNodes#{"nodeId":"%s"}""".format(nodeStatus._1.value)}>
               <span class="curspoint">
               {nodeInfo.hostname}
               </span>
               </a> &
               "#severity *" #> ReportType.getSeverityFromStatus(nodeStatus._2) &
               ".unfoldable [class+]" #> ReportType.getSeverityFromStatus(nodeStatus._2).replaceAll(" ", "")
               )(nodeLineXml)
       }
     case x:EmptyBox =>
       logger.error( (x?~! "An error occured when trying to load node %s".format(nodeStatus._1.value)),x)
       <div class="error">Node with ID "{nodeStatus._1.value}" is invalid</div>
     }
     nodeReport ++ showNodeReports(rest)
     }
    }
   def showReportDetail(nodeReports : ComponentValueRuleStatusReport) : NodeSeq= {
     def showNodeReport(nodeid:NodeId):NodeSeq = {
         nodeInfoService.getNodeInfo(nodeid) match {
         case Full(nodeInfo)  => {
             val message = nodeReports.nodesreport.filter(_._1==nodeid).flatMap(_._3)
             ( "#node *" #>
               <a class="unfoldable" href={"""secure/nodeManager/searchNodes#{"nodeId":"%s"}""".format(nodeid)}>
               <span class="curspoint">
               {nodeInfo.hostname}
               </span>
               </a>  &
               ".unfoldable [class+]" #> ReportType.getSeverityFromStatus(nodeReports.cptValueReportType).replaceAll(" ", "")&
               "#component *" #> nodeReports.component &
               "#severity *" #> Text(ReportType.getSeverityFromStatus(nodeReports.cptValueReportType)) &
               "#value *" #> nodeReports.componentValue &
               "#message *" #>  <ul>{message.map(msg => <li>{msg}</li>)}</ul> 
             ) ( reportLineXml )
               
       }
     case x:EmptyBox =>
       logger.error( (x?~! "An error occured when trying to load node %s".format(nodeid)),x)
       <div class="error">Node with ID "{nodeid}" is invalid</div>
     }
     }
     val reports = nodeReports.nodesreport
     val nodes = reports.map(_._1).distinct
     val nodeStatuses = nodes.map(node => (node,ReportType.getWorseType(reports.filter(_._1==node).map(stat => stat._2)),reports.filter(_._1==node).flatMap(stat => stat._3).toList))
     nodeStatuses.toList match {
     case Nil =>  NodeSeq.Empty
     case nodes => nodes.flatMap(node =>showNodeReport(node._1))
     }
    }
    def showReportDetails(nodeReport : Seq[ComponentValueRuleStatusReport], message:String, id:String = "reportsGrid" ) : NodeSeq = {
 val report =      nodeReport.filterNot(_.cptValueReportType == SuccessReportType)
     if (report.size>0)
     ( "#reportLine" #>
            <div class="reportLine ui-corner-top"><b> {message} </b>{report.map(showReportDetail(_))}</div>
     )(reportsGridXml(id)) 
     else 
      ( "#reportLine" #>
      <div class="reportLine ui-corner-top"><b> All reports message are </b>{nodeReport.map(showReportDetail(_))}</div>
     )(reportsGridXml(id)) 
    }

    def treatmissingreports(nodeReport : Seq[ComponentValueRuleStatusReport]) : NodeSeq = {
      val missingreports = nodeReport.map(value => value.copy(reports =  value.reports.filter(report => report._3.contains("Report should have been received.")))).filter(_.reports.size!=0)
      val unexpectedreports = nodeReport.map(value => value.copy(reports =  value.reports.filter(report => report._3.contains("Report should have not been received")))).filter(_.reports.size!=0)
      (if (missingreports.size !=0)(
          <div>{showReportDetails(missingreports,"Those reports should have been received","missing")}</div>      <hr class="spacer" />    <div class="missing_pagination paginatescala">
            <div id="missing_paginate_area"/>
          </div>
          <hr class="spacer"/>
          <br/>) else NodeSeq.Empty) ++ 
      (if (unexpectedreports.size !=0)(
          <div>{showReportDetails(unexpectedreports,"Those reports should have not been received","unexpected")}</div>      <hr class="spacer" />    <div class="unexpected_pagination paginatescala">
            <div id="unexpected_paginate_area"/>
          </div> 
          <hr class="spacer"/>
          <br/>) else NodeSeq.Empty)
    }
    def reportsGridXml(id:String = "reportsGrid") : NodeSeq = {
    <table id={id}  cellspacing="0" style="clear:both">
      <thead>
        <tr class="head">
          <th>Node<span/></th>
          <th >Component<span/></th>
          <th >Value<span/></th>
          <th >Message <span/></th>
         <th >Severity<span/></th>
        </tr>
      </thead>
      <tbody>
        <div id="reportLine"/>
      </tbody>
    </table>
  }

  def nodeGridXml : NodeSeq = {
    <table id="nodeReportGrid"  cellspacing="0">
      <thead>
        <tr class="head">
          <th>Node<span/></th>
         <th >Severity<span/></th>
        </tr>
      </thead>
      <tbody>
        <div id="reportLine"/>
      </tbody>
    </table>
  }
   def reportLineXml : NodeSeq = {
    <tr class="unfoldable">
      <td id="node"></td>
      <td id="component"></td>
      <td id="value"></td>
      <td id="message"></td>
      <td id="severity"></td>
    </tr>
  }
   
  def nodeLineXml : NodeSeq = {
    <tr class="unfoldable">
      <td id="node"></td>
      <td id="severity"></td>
    </tr>
  }
 
  val batch = directivebynode match {
       case DirectiveRuleStatusReport(directiveId,components,_) => 
         components.flatMap(_.componentValues)
       case ComponentRuleStatusReport(directiveId,component,values,_) =>
         values
       case value : ComponentValueRuleStatusReport =>
         Seq(value)
  }
   <div class="simplemodal-title">
    <h1>Node compliance detail</h1>
    <hr/>
  </div>++{

      <div class="simplemodal-content" style="height:300px;overflow-y:auto;" > { bind("lastReportGrid",reportTemplate,
        "detail" ->{  directivebynode match {
    case d:DirectiveRuleStatusReport =>
      val directive = directiveRepository.getDirective(d.directiveId) 
        <div>
                      <ul>
                        <li> <b>Rule:</b> {rule.name}</li>
                        <li><b>Directive:</b> {directive.map(_.name).getOrElse("can't find directive name")}</li>
                      </ul>
                    </div>   
     case c:ComponentRuleStatusReport =>
      val directive = directiveRepository.getDirective(c.directiveid)
       <div>
                      <ul>
                        <li> <b>Rule:</b> {rule.name}</li>
                        <li><b>Directive:</b> {directive.map(_.name).getOrElse("can't find directive name")}</li>
                        <li><b>Component:</b> {c.component}</li>
                      </ul>
                    </div>++  ( "#reportLine" #>
     { showNodeReports(c.nodesreport)}
     )(nodeGridXml)
      case ComponentValueRuleStatusReport(directiveId,component,value,_,_) =>
      val directive = directiveRepository.getDirective(directiveId)
      <div>
                      <ul>
                        <li> <b>Rule:</b> {rule.name}</li>
                        <li><b>Directive:</b> {directive.map(_.name).getOrElse("can't find directive name")}</li>
                        <li><b>Component:</b> {component}</li>
                        <li><b>Value:</b> {value}</li>
                      </ul>
                    </div> 
                        } },
      "lines"   -> showReportDetails(batch,"You shoud look for all this reports message"),
      "missing" -> treatmissingreports(batch)
         )
      }
      <hr class="spacer" />
 { ( "#reportLine" #>
     { showNodeReports(directivebynode.nodesreport)}
     )(nodeGridXml)}
       <div class="nodeReportGrid_pagination paginatescala">
     <div id="nodeReportGrid_paginate_area"/>
     </div>
           <hr class="spacer"/>
           <br/>
      </div>

  
  } ++
     <div class="simplemodal-bottom">
    <hr/>
    <div class="popupButton">
      <span>
        <button class="simplemodal-close" onClick="return false;">
          Close
        </button>
      </span>
    </div>
  </div>
  }
  def jsVarNameForId(tableId:String) = "oTable" + tableId
  val htmlId_rulesGridZone = "rules_grid_zone"
  val htmlId_rulesGridId = "grid_" + htmlId_rulesGridZone
  val tableId_reportsPopup = "reportsGrid"
  val htmlId_reportsPopup = "popup_" + htmlId_rulesGridZone
  val htmlId_modalReportsPopup = "modal_" + htmlId_rulesGridZone
  
  private[this] def showPopup(directiveStatus: RuleStatusReport) : JsCmd = {
    val popupHtml = createPopup(directiveStatus)
    SetHtml(htmlId_reportsPopup, popupHtml) &
      JsRaw("""
        /* Formating function for row details */
        function fnFormatDetails ( id ) {
          var sOut = '<div id="'+id+'" class="reportDetailsGroup"/>';
          return sOut;
        }
      """.replaceAll("#table_var#",jsVarNameForId(tableId_reportsPopup))
    ) & OnLoad(
        JsRaw("""
          /* Event handler function */
         $('#nodeReportGrid').dataTable({
            "bAutoWidth": false,
            "bFilter" : false,
            "bPaginate" : true,
            "bLengthChange": false,
            "sPaginationType": "full_numbers",
            "bJQueryUI": false,
            "aaSorting": [[ 3, "asc" ]],
            "iDisplayLength" : 5,
            "aoColumns": [
              { "sWidth": "150px" },
              { "sWidth": "150px" }
            ]
          });
            $('#%1$s').dataTable({
            "bAutoWidth": false,
            "bFilter" : true,
            "bPaginate" : true,
            "bLengthChange": false,
            "sPaginationType": "full_numbers",
            "bJQueryUI": false,
            "aaSorting": [[ 5, "asc" ]],
            "bSearch": "",
            "aoColumns": [
              { "sWidth": "150px" },
              { "sWidth": "150px" },
              { "sWidth": "150px" },
              { "sWidth": "250px" },
              { "sWidth": "50px" }
            ]
           });
          $('.dataTables_filter input').attr("placeholder", "Search");
          $('.dataTables_filter').contents().filter(function(){
            return this.nodeType == this.TEXT_NODE;
            })​.remove();​

          $('#missing').dataTable({
            "bAutoWidth": false,
            "bFilter" : false,
            "bPaginate" : true,
            "bLengthChange": false,
            "sPaginationType": "full_numbers",
            "bJQueryUI": false,
            "aaSorting": [[ 5, "asc" ]],
            "aoColumns": [
              { "sWidth": "150px" },
              { "sWidth": "150px" },
              { "sWidth": "150px" },
              { "sWidth": "250px" },
              { "sWidth": "50px" }
            ]
          });
            $('#unexpected').dataTable({
            "bAutoWidth": false,
            "bFilter" : false,
            "bPaginate" : true,
            "bLengthChange": false,
            "sPaginationType": "full_numbers",
            "bJQueryUI": false,
            "aaSorting": [[ 5, "asc" ]],
            "aoColumns": [
              { "sWidth": "150px" },
              { "sWidth": "150px" },
              { "sWidth": "150px" },
              { "sWidth": "250px" },
              { "sWidth": "50px" }
            ]
          });moveFilterAndFullPaginateArea('#%1$s');
            moveFilterAndFullPaginateArea('#nodeReportGrid');
            moveFilterAndFullPaginateArea('#missing');
            moveFilterAndFullPaginateArea('#unexpected');""".format( tableId_reportsPopup)
        ) //&  initJsCallBack(tableId)
    ) &
    JsRaw( """ createPopup("%s",600,750)
     """.format(htmlId_modalReportsPopup))
  }
  
}
  
