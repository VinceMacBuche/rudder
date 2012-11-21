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

import com.normation.eventlog.EventLogType
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import scala.xml._
import net.liftweb.http.js.JE._
import com.normation.rudder.domain.eventlog._
import com.normation.eventlog.EventLogType


class EventFilter (
    targetfilter: String
    ) extends DispatchSnippet with Loggable {
    
    def dispatch = { 
    case "showForm" => { _ => <div id="filterZone">{filter()}</div> }
  }

    val f = EventLogFilter

    
    def filter(filters:List[EventLogFilter]= f.configuration :: f.inventory :: f.intern :: Nil) : NodeSeq = {
      <div id="filter">
      <b> Display event types :</b> 
        <ul style="padding-left:50px">
        {filters.map(_.display(targetfilter))}
        </ul>
      <div>{SHtml.ajaxButton("More filters...",()=>Replace("filter",filter2()))}</div>
      </div>++Script(JsRaw("var filter = [];"))
    }
    
    def filter2(filters:List[EventLogFilter]= f.Rule :: f.Directive :: f.Node ::  f.intern :: Nil) : NodeSeq = {
      <div id="filter">
      <b> Display event types :</b> 
        <ul style="padding-left:50px">
        {filters.map(_.display(targetfilter))}
        </ul>
      <div>{SHtml.ajaxButton("Less filters...",()=>Replace("filter",filter()))}</div>
      </div>++Script(JsRaw("var filter = [];"))
    }

}


case class EventLogFilter(
    name : String
  , eventTypes : List[EventLogType]
    ){
  
  def filter = eventTypes.map(evtT => S.?("rudder.log.eventType.names."+evtT.serialize)).mkString("['","','","']")
  
  def onclick(target:String) = ("onclick",""" var tab = %s;
      if (this.checked){  tab.forEach(function(x) { if(filter.indexOf(x)==-1) filter.push(x); })
      }
  else  { tab.forEach(function(x) {
     filter = filter.filter(function(y){ return y!=x; }); } ) }
      $('#%s').dataTable().fnFilter(filter.join('|'),3,true,false);
      """.format(filter,target))
  def display(target:String) = <li><span> {name} : { SHtml.checkbox(false, (bool) => (),onclick(target))}</span></li>
}

object EventLogFilter{
  def apply(name:String,eventType:EventLogType):EventLogFilter= EventLogFilter(name,List(eventType))
  implicit def toEventType(evtFilters:List[EventLogFilter]):List[EventLogType] = evtFilters.flatMap(_.eventTypes)  

  /*
   * Configuration filters
   */
  val addRule = EventLogFilter("Add",AddRuleEventType)  
  val modRule = EventLogFilter("Modify",ModifyRuleEventType)
  val delRule = EventLogFilter("Delete",DeleteRuleEventType)
  val rules = List(addRule,modRule,delRule)
  val Rule = EventLogFilter("Rule",rules)
  val addDirective = EventLogFilter("Add",AddDirectiveEventType)  
  val modDirective = EventLogFilter("Modify",ModifyDirectiveEventType)
  val delDirective = EventLogFilter("Delete",DeleteDirectiveEventType)
  val directives = List(addDirective,modDirective,delDirective)
  val Directive = EventLogFilter("Directive",directives)
  val successfulDep = EventLogFilter("Successful",SuccessfulDeploymentEventType)
  val failedDep = EventLogFilter("Failed",SuccessfulDeploymentEventType)
  val manualDep = EventLogFilter("Manual Start",ManualStartDeployementEventType)
  val deployments = List(successfulDep,failedDep,manualDep)
  val deployment = EventLogFilter("Deployment",deployments)
  val actRedButton = EventLogFilter("Activate",ActivateRedButtonEventType)
  val relRedButton = EventLogFilter("Release",ReleaseRedButtonEventType)
  val redButtons   =  List(actRedButton,relRedButton)
  val RedButton = EventLogFilter("Red button",redButtons)
  val reloadTechLib = EventLogFilter("Reload",ReloadTechniqueLibraryType)
  val modTech = EventLogFilter("Modify",ModifyTechniqueEventType)
  val delTech = EventLogFilter("Delete",DeleteTechniqueEventType)
  val techniques = List(reloadTechLib,modTech,delTech)
  val tech = EventLogFilter("Technique",techniques)
  val clearCache = EventLogFilter("Clear Cache",ClearCacheEventType)
  val policyServer = EventLogFilter("Update policy server",UpdatePolicyServerEventType)
  val confs = List(Rule,Directive,tech,deployment,clearCache,policyServer)
  val configuration = EventLogFilter("Configuration",confs)

  /*
   * Inventory Filters
   */
  val accNode = EventLogFilter("Accept",AcceptNodeEventType)  
  val refNode = EventLogFilter("Refuse",RefuseNodeEventType)
  val delNode = EventLogFilter("Delete",DeleteNodeEventType)
  val nodes = List(accNode,refNode,delNode)
  val Node = EventLogFilter("Node",nodes)  
  val addGroup = EventLogFilter("Add",AddNodeGroupEventType)  
  val modGroup = EventLogFilter("Modify",ModifyNodeGroupEventType)
  val delGroup = EventLogFilter("Delete",DeleteNodeGroupEventType)
  val groups = List(addGroup,modGroup,delGroup)
  val Group = EventLogFilter("Group",groups)
  val inventories = List(Node,Group)
  val inventory = EventLogFilter("Inventory",inventories)
  
  /*
   * Internal Filters
   */
  val login = EventLogFilter("Login",LoginEventType)
  val logout = EventLogFilter("Logout",LogoutEventType)
  val badCred = EventLogFilter("Bad Credentials",BadCredentialsEventType)
  val users = List(login,logout,badCred)
  val user = EventLogFilter("User",users)
  val AppliStart = EventLogFilter("Application Start",ApplicationStartedEventType)
  val autodep = EventLogFilter("Automatic Deployment start",AutomaticStartDeployementEventType)
  val interns = List(user,AppliStart,autodep)
  val intern = EventLogFilter("Internal",interns)
}


/*
rudder.log.eventType.names.ExportGroups=Groups exported (archived)
rudder.log.eventType.names.ImportGroups=Groups imported
rudder.log.eventType.names.ExportTechniqueLibrary=Technique Library exported (archived)
rudder.log.eventType.names.ImportTechniqueLibrary=Technique Library imported
rudder.log.eventType.names.ExportRules=Rules exported (archived)
rudder.log.eventType.names.ImportRules=Rules imported
rudder.log.eventType.names.ExportFullArchive=All configuration exported (archived)
rudder.log.eventType.names.ImportFullArchive=All configuration imported
*/