/*
*************************************************************************************
* Copyright 2012 Normation SAS
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
    case "showForm" => { _ => <div id="filterZone">      <b> Display event types :</b> {filter()}</div>}
  }

    val f = EventLogFilter

      def onclick ="""
           var actualLevel= levels[level];
          if(lesslevel.indexOf(actualLevel)==-1) 
           $('#less').button('disable').blur().removeClass('ui-state-hover');
      else 
           $('#less').button('enable');
          if (morelevel.indexOf(actualLevel)==-1) 
           $('#more').button('disable').blur().removeClass('ui-state-hover');
      else 
                     $('#more').button('enable');
         console.log($('.'+actualLevel));
       $('.'+actualLevel).each(function(i,lev){console.log($(lev));$(lev).css('display','inline');});
        levels.filter(function(x){return x!=actualLevel;}).forEach(function(x){
          console.log( $('.'+x)); $('.'+x).each(function(i,c){$(c).css('display','none');});   
        })
      """
    def filter(filters:List[EventLogFilter]= f.configuration :: f.inventory :: f.intern :: Nil) : NodeSeq = {
      <fieldset id="filter" class="filterfieldset filtermainfieldset"><legend><b>filter by event type</b></legend>
            <span>{SHtml.submit("Less filters...",()=>(),("id","less"),("type","button"),("onclick","level = level-1;"+onclick))}
      {SHtml.submit("More filters...",()=>(),("id","more"),("type","button"),("onclick","level = level+1;"+onclick))}</span>
        <ul style="padding-left:25px;display:inline;" >
        {filters.map(filter => filter.display(targetfilter))}
        </ul>
      </fieldset> ++Script(JsRaw("  $('#less').button();  $('#more').button(); var filter = []; var level=0; var levels=['level1','level2','level3']; var lesslevel=['level2','level3'];var morelevel=['level1','level2'];"+onclick))
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
  
  def childs = EventLogFilter.childs(this)
  def parent = EventLogFilter.parent(this)
  def display(target:String) = {
      def level1 = <span> {name} : { SHtml.checkbox(false, (bool) => (),onclick(target))}</span>
      def level2 = { childs match {
        case Nil => level1
        case _ => <fieldset class="filterfieldset"><legend>{name}</legend><ul id={name}>{childs.flatMap(child => <li>{child.oneLevel(target)}</li>)}</ul></fieldset>
      }
      }
      def level3 = {childs match {
        case Nil => level1
        case _ =>   <fieldset class="filterfieldset"><legend>{name}</legend><ul id={name}>{childs.map(_.twoLevel(target))}</ul></fieldset>
      }
      } 
    
    <li class="level1" style="vertical-align:top;"> {level1} </li>    <li class="level2" style="vertical-align:top;" > {level2} </li>    <li class="level3" style="vertical-align:top;">{level3} </li>
  }
  def oneLevel(target:String) = <span> {name} : { SHtml.checkbox(false, (bool) => (),onclick(target))}</span>
  def twoLevel(target:String) = childs match {
    case Nil => <li>{oneLevel(target)}</li>
    case _   => <li id={name}><b>{name} :</b><ul style="display:inline">{childs.flatMap( child => <li style="display:inline">{child.oneLevel(target)}</li>)}</ul></li>
  }
}

case object EventLogFilter{
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
  val failedDep = EventLogFilter("Failed",FailedDeploymentEventType)
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
  
  def parent(filter:EventLogFilter): EventLogFilter = filter match {
    case _ if confs.contains(filter) => configuration
    case _ if techniques.contains(filter) => tech
    case _ if deployments.contains(filter) => deployment
    case _ if directives.contains(filter) => Directive
    case _ if rules.contains(filter) => Rule
    
    case _ if inventories.contains(filter) => inventory
    case _ if groups.contains(filter) => Group
    case _ if nodes.contains(filter) => Node
    
    case _ if interns.contains(filter) => intern
    case _ if users.contains(filter) => user
    case _ => filter
  }
  
  def childs(filter:EventLogFilter): List[EventLogFilter] = filter match {
    case this.configuration => confs
    case this.tech => techniques
    case this.deployment => deployments
    case this.Directive => directives
    case this.Rule => rules
    case this.inventory =>  inventories
    case this.Node => nodes
    case this.Group => groups
    case this.intern => interns
    case this.user => users
    case _ => Nil
  }
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