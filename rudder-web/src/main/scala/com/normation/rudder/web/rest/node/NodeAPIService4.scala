/*
*************************************************************************************
* Copyright 2013 Normation SAS
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

package com.normation.rudder.web.rest.node


import com.normation.inventory.domain.AcceptedInventory
import com.normation.utils.StringUuidGenerator
import com.normation.rudder.web.rest.RestExtractorService
import com.normation.rudder.web.rest.RestDataSerializer
import com.normation.inventory.domain.NodeId
import com.normation.inventory.domain.FullInventory
import net.liftweb.json._
import net.liftweb.common._
import com.normation.rudder.web.rest.RestUtils._
import net.liftweb.json.JsonDSL._
import com.normation.inventory.ldap.core.LDAPFullInventoryRepository
import net.liftweb.http.Req
import com.normation.inventory.domain.PhysicalMachineType
import com.normation.inventory.domain.VirtualMachineType
import org.joda.time.format.ISODateTimeFormat
import com.normation.inventory.domain.InventoryStatus
import com.normation.inventory.domain.PendingInventory
import com.normation.inventory.domain.RemovedInventory

class NodeApiService4 (
    inventoryRepository : LDAPFullInventoryRepository
  , uuidGen           : StringUuidGenerator
  , restExtractor     : RestExtractorService
  , restSerializer    : RestDataSerializer
) extends Loggable {

  import restSerializer._

  def getNodeDetails(nodeId : NodeId, detailLevel : NodeDetailLevel, state: InventoryStatus) = {
    inventoryRepository.get(nodeId,state).map(serializeInventory(_, detailLevel))
  }

  def nodeDetailsWithStatus(nodeId : NodeId, detailLevel : NodeDetailLevel, state: InventoryStatus, req:Req) = {
    implicit val prettify = restExtractor.extractPrettify(req.params)
    implicit val action = "nodeDetails"
    getNodeDetails(nodeId,detailLevel,state) match {
        case Full(inventory) =>
          toJsonResponse(Some(nodeId.value), ( "nodes" -> JArray(List(inventory))))
        case eb: EmptyBox =>
          val message = (eb ?~ (s"Could not find Node ${nodeId.value}")).msg
          toJsonError(Some(nodeId.value), message)
      }
  }


  def nodeDetailsGeneric(nodeId : NodeId, detailLevel : NodeDetailLevel, req:Req) = {
    implicit val prettify = restExtractor.extractPrettify(req.params)
    implicit val action = "nodeDetails"
    getNodeDetails(nodeId,detailLevel,AcceptedInventory) match {
        case Full(inventory) =>
          toJsonResponse(Some(nodeId.value), ( "nodes" -> JArray(List(inventory))))
        case eb: EmptyBox =>
          getNodeDetails(nodeId,detailLevel,PendingInventory) match {
            case Full(inventory) =>
              toJsonResponse(Some(nodeId.value), ( "nodes" -> JArray(List(inventory))))
            case eb: EmptyBox =>
              getNodeDetails(nodeId,detailLevel,RemovedInventory) match {
                case Full(inventory) =>
                  toJsonResponse(Some(nodeId.value), ( "nodes" -> JArray(List(inventory))))
                case eb: EmptyBox =>
                  val message = (eb ?~ (s"Could not fetch Node ${nodeId.value}")).msg
                  toJsonError(Some(nodeId.value), message)
              }
          }
    }
  }
}



object NodeDetailLevel {

  val fieldToJson : Map[String, FullInventory => JValue] = {

    val dateFormatter = ISODateTimeFormat.dateTime
    (
       ( "id"       -> ((ft : FullInventory) => JString(ft.node.main.id.value)))
    :: ( "hostname" -> ((ft : FullInventory) => JString(ft.node.main.hostname)))
    :: ( "status"   -> ((ft : FullInventory) => JString(ft.node.main.status.name)))
    :: ( "description" -> ((ft : FullInventory) => JString(ft.node.description.getOrElse(""))))
    :: ( "policyServerId"   -> ((ft : FullInventory) => JString(ft.node.main.policyServerId.value)))
    :: ( "OS" ->
         ( ( ft : FullInventory ) =>
           ( "type" -> ft.node.main.osDetails.os.kernelName) ~
           ( "name" -> ft.node.main.osDetails.os.name) ~
           ( "version" -> ft.node.main.osDetails.version.value) ~
           ( "servicePack" -> ft.node.main.osDetails.servicePack.getOrElse("N/A")) ~
           ( "KernelVersion" -> ft.node.main.osDetails.kernelVersion.value)
       ) )
    :: ("machine" ->

         ( ( ft : FullInventory ) => {
           val (machineType,provider) = ft.machine.map(_.machineType match {
             case PhysicalMachineType => ("Physical",None)
             case VirtualMachineType(kind) => ("Virtual",Some(kind))
           }).getOrElse(("No machine Inventory",None))

           ( "type" -> machineType) ~
           ( "provider" -> provider.map(_.name).getOrElse("N/A")) ~
           ( "id" -> ft.machine.map(_.id.value).getOrElse("N/A"))
         }
       ) )
    :: ("ram"   -> ((ft : FullInventory) => JString(ft.node.ram.map(_.toStringMo).getOrElse("N/A"))))
    :: ("ipAdresses" ->
         ( ( ft : FullInventory ) => {
           val ips =ft.node.serverIps.map(ip => JString(ip))
           JArray(ips.toList)
         }
       ) )
    :: ("managementTechnology" ->
         ( ( ft : FullInventory ) => {
           val agents = ft.node.agentNames.map{ agent => ("name" -> agent.fullname) ~ ("version" -> "N/A")}
           JArray(agents.toList)
         }
       ) )
    :: ( "architectureDescription" ->
         ( ( ft : FullInventory ) => {
           JString(ft.node.archDescription.getOrElse("N/A"))
         }
       ) )
    :: ("lastInventoryDate" ->
         ( ( ft : FullInventory ) => {
           JString(ft.node.inventoryDate.map(_.toString(dateFormatter)).getOrElse("N/A"))
         }
       ) )
    :: Nil
    ).toMap

  }
}



sealed trait NodeDetailLevel {
  def fields : Set[String]
}

case object Minimal extends NodeDetailLevel {
  val fields = Set("id", "hostname", "status")
}
case object Default extends NodeDetailLevel {
  val fields = Minimal.fields ++ Set("description", "policyServerId", "OS", "machine", "ram", "ipAdresses", "managementTechnology", "architectureDescription", "lastInventoryDate")
}

