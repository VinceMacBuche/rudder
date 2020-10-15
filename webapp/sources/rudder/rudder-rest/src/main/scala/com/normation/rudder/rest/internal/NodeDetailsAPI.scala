package com.normation.rudder.rest.internal

import com.normation.inventory.domain.AcceptedInventory
import com.normation.inventory.domain.Software
import com.normation.inventory.services.core.ReadOnlySoftwareDAO
import com.normation.rudder.domain.nodes.NodeInfo
import com.normation.rudder.domain.policies.GlobalPolicyMode
import com.normation.rudder.domain.policies.PolicyModeOverrides.Always
import com.normation.rudder.domain.policies.PolicyModeOverrides.Unoverridable
import com.normation.rudder.reports.execution.AgentRunWithNodeConfig
import com.normation.rudder.reports.execution.RoReportsExecutionRepository
import com.normation.rudder.rest.RestExtractorService
import com.normation.rudder.services.nodes.NodeInfoService
import com.normation.rudder.web.components.DateFormaterService
import com.typesafe.config.ConfigRenderOptions
import net.liftweb.common.Box
import net.liftweb.common.Loggable
import net.liftweb.http.JsonResponse
import net.liftweb.http.LiftResponse
import net.liftweb.http.Req
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonAST.JArray
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.parse

class NodeDetailsAPI (
  nodeInfoService: NodeInfoService
  , restExtractor : RestExtractorService
  , reportsExecutionRepository: RoReportsExecutionRepository
  , getGlobalMode : () => Box[GlobalPolicyMode]
  , readOnlySoftwareDAO: ReadOnlySoftwareDAO

) extends  RestHelper with  Loggable {


  def serialize(agentRunWithNodeConfig: Option[AgentRunWithNodeConfig], globalPolicyMode: GlobalPolicyMode, nodeInfo : NodeInfo, properties : List[String], softs: Seq[Software]) = {
    import net.liftweb.json.JsonDSL._

    val (policyMode,explanation) =
      (globalPolicyMode.overridable,nodeInfo.policyMode) match {
        case (Always,Some(mode)) =>
          (mode,"<p>This mode is an override applied to this node. You can change it in the <i><b>node's settings</b></i>.</p>")
        case (Always,None) =>
          val expl = """<p>This mode is the globally defined default. You can change it in <i><b>settings</b></i>.</p><p>You can also override it on this node in the <i><b>node's settings</b></i>.</p>"""
          (globalPolicyMode.mode, expl)
        case (Unoverridable,_) =>
          (globalPolicyMode.mode, "<p>This mode is the globally defined default. You can change it in <i><b>Settings</b></i>.</p>")
      }
    (  ("name" -> nodeInfo.hostname)
      ~  ("policyServerId" -> nodeInfo.policyServerId.value)
      ~  ("policyMode" -> policyMode.name)
      ~  ("explanation" -> explanation)

      ~  ("agentVersion" -> nodeInfo.agentsName.headOption.flatMap(_.version.map(_.value)))
      ~  ("id" -> nodeInfo.id.value)
      ~  ("ram" -> nodeInfo.ram.map(_.toStringMo()))
      ~  ("machineType" -> nodeInfo.machine.map(_.machineType.toString))
      ~  ("os" -> nodeInfo.osDetails.fullName)
      ~  ("state" -> nodeInfo.state.name)
      ~  ("ipAddresses" -> nodeInfo.ips)
      ~  ("lastRun" -> agentRunWithNodeConfig.map(d => DateFormaterService.getDisplayDate(d.agentRunId.date)).getOrElse("Never"))
     // ~  ("software" -> JObject(softs.toList.map(s => JField(s.name.getOrElse(""), JString(s.version.map(_.value).getOrElse("N/A"))))))
      ~  ("properties" -> JObject(nodeInfo.properties.filter(p => properties.contains(p.name)).map(p => JField(p.name, parse(p.value.render(ConfigRenderOptions.concise()) ) )) ))
      )
  }
  def requestDispatch: PartialFunction[Req, () => Box[LiftResponse]] = {
    case Get(Nil, req) =>
      import com.normation.box._
      for {
        nodes <- nodeInfoService.getAll()
        runs <- reportsExecutionRepository.getNodesLastRun(nodes.keySet)
        globalMode <- getGlobalMode()
        //softs <- readOnlySoftwareDAO.getSoftwareByNode(nodes.keySet,AcceptedInventory).toBox
      } yield {
        JsonResponse(JArray(nodes.values.toList.map(n => serialize(runs.get(n.id).flatten,globalMode,n, req.params.get("properties").getOrElse(Nil), Seq()))))
      }
  }


  serve("secure" / "api" / "nodeDetails" prefix requestDispatch)
}
