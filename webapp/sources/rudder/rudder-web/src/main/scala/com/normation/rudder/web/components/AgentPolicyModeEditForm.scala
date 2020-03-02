package com.normation.rudder.web.components

import net.liftweb.http.DispatchSnippet
import net.liftweb.common._
import net.liftweb.http.js._
import JsCmds._
import JE._
import com.normation.inventory.domain.NodeId
import com.normation.rudder.web.ChooseTemplate

import scala.xml.NodeSeq

class AgentPolicyModeEditForm (nodeId : Option[NodeId]) extends DispatchSnippet with Loggable  {

  // Html template
  def agentPolicyModeTemplate: NodeSeq = ChooseTemplate(
      List("templates-hidden", "components", "ComponentAgentPolicyMode")
    , "agentpolicymode-form"
  )

  def dispatch = {
    case "cfagentPolicyModeConfiguration" => (xml) => cfagentPolicyModeConfiguration
  }

  def cfagentPolicyModeConfiguration : NodeSeq = {

    import net.liftweb.util.Helpers._
    val res = nodeId match {
      case Some(nodeId) =>s"""nodeId = "${nodeId.value}"; """
      case None => ""
    }
    ("ng-controller=auditmodeCtrl [ng-init]" #> res ). apply(agentPolicyModeTemplate++

    Script(OnLoad(JsRaw("angular.bootstrap('#auditMode', ['auditmode']);"))))
  }
}
