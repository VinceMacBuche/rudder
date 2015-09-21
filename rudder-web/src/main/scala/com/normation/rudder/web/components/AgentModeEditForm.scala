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

package com.normation.rudder.web.components

import bootstrap.liftweb.RudderConfig
import net.liftweb.http.DispatchSnippet
import net.liftweb.common._
import com.normation.rudder.domain.policies.Directive
import net.liftweb.http.{SHtml,S}
import scala.xml._
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.js._
import JsCmds._
import com.normation.rudder.web.components.popup.CreateOrCloneRulePopup
import JE._
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import net.liftweb.http.Templates
import com.normation.rudder.reports.AgentRunInterval
import net.liftweb.util.PassThru
import net.liftweb.util.ClearNodes
import com.normation.rudder.reports.AgentMode
import ComplianceModeEditForm._
import com.normation.eventlog.EventActor
import com.normation.rudder.web.model.CurrentUser
import com.normation.rudder.reports.GlobalAgentMode
import com.normation.rudder.reports.VerifyMode
import com.normation.rudder.reports.GlobalAgentMode
import net.liftweb.json.JsonAST._
import net.liftweb.json.Printer
/**
 * Component to display and configure the agent Mode (and it's heartbeat)
 */

class AgentModeEditForm (
    agentModeBox : Box[AgentMode]
  , saveCallback: (AgentMode) => Box[Unit]
  , startNewPolicyGeneration: () => Unit
  , globalBox : Box[GlobalAgentMode]
) extends DispatchSnippet with Loggable  {

  // Html template
  def templatePath = List("templates-hidden", "components", "ComponentAgentMode")
  def template() =  Templates(templatePath) match {
     case Empty | Failure(_,_,_) =>
       error(s"Template for Compliance mode configuration not found. I was looking for ${templatePath.mkString("/")}.html")
     case Full(n) => n
  }
  def agentScheduleTemplate = chooseTemplate("property", "agentmode", template)

  def dispatch = {
    case "agentMode" => (xml) => agentModeConfiguration
  }

  def submit(agentMode:String) = {
    parseAgentMode(agentMode) match {
      case eb:EmptyBox =>
        val e = eb ?~! s"Error when trying to parse user data: '${agentMode}'"
        logger.error(e.messageChain)
        S.error("agentModeMessage", e.messageChain)
      case Full(agentMode) =>
        saveCallback(agentMode)  match {
          case eb:EmptyBox =>
            val e = eb ?~! s"Error when trying to store in base new agent schedule: '${agentMode}'"
            logger.error(e.messageChain)
            S.error("agentModeMessage", e.messageChain)

          case Full(success) =>
            // start a promise generation, Since we check if there is change to save, if we got there it mean that we need to redeploy
            startNewPolicyGeneration()
            S.notice("agentModeMessage", "Global agent mode updated")
        }
    }
  }

  /**
   * Parse a json input into a cf-agent Scedule
   */
  def parseAgentMode(s: String): Box[AgentMode] = {

    def parseGlobal(jsonMode : JObject, mode: AgentMode) : Box[AgentMode] = {
      jsonMode.values.get("allow_override") match {
        case Some(JBool(bool)) => Full(GlobalAgentMode(mode,bool))
        case Some(allow_override : Boolean) => Full(GlobalAgentMode(mode,allow_override))
        case Some(json : JValue) => Failure(s"'${render(json)}' is not a valid value for agent mode 'override' attribute")
        // Should not happen, values in lift json are only JValues, but since we type any we have to add it unless we will have a warning
        case Some(any) => Failure(s"'${any}' is not a valid value for agent mode 'override' attribute")
        case None => Full(mode)
      }
    }

    def parseMode(jsonMode: JObject) : Box[AgentMode]= {
      logger.info(jsonMode.values.get("name"))
      jsonMode.values.get("name") match {
        case Some(JString(mode)) => AgentMode(mode)
        case Some(mode : String) => AgentMode(mode)
        case Some(json : JValue) => Failure(s"'${(json)}' is not a valid value for agent mode 'name' attribute")
        // Should not happen, values in lift json are only JValues, but since we type any we have to add it unless we will have a warning
        case Some(any) => Failure(s"'${any}' is not a valid value for agent mode 'name' attribute")
        case None => Failure("agent mode 'name' parameter must not be empty ")
      }
    }
    import net.liftweb.json.parse
    val json = parse(s)
     json match {
      case obj : JObject =>

        for {
           mode <- parseMode(obj)
           finalMode <- parseGlobal(obj, mode)
        } yield {
          finalMode
        }
      case _ =>
        Failure(s"Could not parse ${s} as a valid agent mode")
     }
  }

  def serializeAgentMode(agentMode : AgentMode) : (JValue,Boolean) = {
    import net.liftweb.json.JsonDSL._
    agentMode match {
      case GlobalAgentMode(mode,overrideValue) => (( "name" -> mode.value )~ (  "allow_override" -> overrideValue), false)
      case mode =>  (( "name" -> mode.value ),true)
    }
  }

  def renderJson (json:JValue)= {
    Printer.compact(render(json))
  }
  def agentModeConfiguration = {

    import net.liftweb.json.JsonAST._
    val init = (for {
      agentMode <- agentModeBox
      globalMode <- globalBox
    } yield {
        val (jsonAgent,nodePage) = serializeAgentMode(agentMode)
        val (jsonGlobal,_) = serializeAgentMode(globalMode)

        val res = Printer.compact(render(jsonAgent))
        val callback = AnonFunc("agentMode",SHtml.ajaxCall(JsVar("agentMode"), submit))
        s"""agentModeForm( ${renderJson(jsonAgent)}, ${renderJson(jsonGlobal)},  ${nodePage} ,${callback.toJsCmd} ); """

    }) match{
      case Full(initScheduleParam) =>
        ( "#agentMode *+" #> Script(OnLoad(JsRaw(initScheduleParam)) & Noop) )
      case eb:EmptyBox =>
        val e = eb ?~! "Error when retrieving agent mode from the database"
        logger.error(e.messageChain)
        e.rootExceptionCause.foreach { ex => logger.error(s"Root exception was: ${ex}") }

        ( "#agentMode" #> "Error when retrieving agent mode from the database. Please, contact an admin or try again later"  )
    }

    init(agentScheduleTemplate)
   }
}
