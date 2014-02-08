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

package com.normation.rudder.domain.policies

import com.normation.rudder.domain.nodes.NodeGroupId
import com.normation.inventory.domain.NodeId
import com.normation.utils.HashcodeCaching
import com.normation.rudder.domain.nodes.NodeGroup
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.util.JSONParser


/**
 * A target is either
 * - a Group of Node (static or dynamic),
 * - a list of Node
 * - a special (system) target ("all", "policy server", etc)
 * - a specific node
 */
sealed abstract class RuleTarget {
  def target:String
  def toJson : JValue = JString(target)
}

sealed trait NonGroupRuleTarget extends RuleTarget

object GroupTarget { def r = "group:(.+)".r }
final case class GroupTarget(groupId:NodeGroupId) extends RuleTarget with HashcodeCaching {
  override def target = "group:"+groupId.value
}

//object NodeTarget { def r = "node:(.+)".r }
//case class NodeTarget(nodeId:NodeId) extends RuleTarget {
//  override def target = "node:"+nodeId.value
//}

object PolicyServerTarget { def r = "policyServer:(.+)".r }
final case class PolicyServerTarget(nodeId:NodeId) extends NonGroupRuleTarget with HashcodeCaching {
  override def target = "policyServer:"+nodeId.value
}

final case object AllTarget extends NonGroupRuleTarget {
  override def target = "special:all"
  def r = "special:all".r
}

final case object AllTargetExceptPolicyServers extends NonGroupRuleTarget {
  override def target = "special:all_exceptPolicyServers"
  def r = "special:all_exceptPolicyServers".r
}

case class TargetIntersection ( targets : List [RuleTarget]) extends RuleTarget {
  override val toJson : JValue = ( "and" -> targets.map(_.toJson))
  val target = toJson.toString

  override def toString = targets.map(_.toString).mkString("( "," and ", " )")

}

case class TargetUnion ( targets : List [RuleTarget]) extends RuleTarget {
  override val toJson : JValue = ( "or" -> targets.map(_.toJson))
  val target = toJson.toString
  override def toString = targets.map(_.toString).mkString("( "," or ", " )")

}

case class TargetExclusion ( excludedTarget : RuleTarget) extends RuleTarget {
  override val toJson : JValue = ( "not" -> excludedTarget.toJson )
  val target = toJson.toString
  override def toString = "not ( " +target.toString()+" )"

}

object RuleTarget {

  def unser(s:String) : Option[RuleTarget] = {
    s match {
      case GroupTarget.r(g) => Some(GroupTarget(NodeGroupId(g)))
//      case NodeTarget.r(s) => Some(NodeTarget(NodeId(s)))
      case PolicyServerTarget.r(s) => Some(PolicyServerTarget(NodeId(s)))
      case AllTarget.r() => Some(AllTarget)
      case AllTargetExceptPolicyServers.r() => Some(AllTargetExceptPolicyServers)
      case s =>  parse(s) match {
        case JString(value) => unser(s)
        case JObject(JField("and", JArray()) :: Nil)

      }
    }
  }
}

/** common information on a target */

case class RuleTargetInfo(
    target     : RuleTarget
  , name       : String
  , description: String
  , isEnabled  : Boolean
  , isSystem   : Boolean
) extends HashcodeCaching

///// the full version with all information /////

sealed trait FullRuleTarget {
  def target: RuleTarget
}

final case class FullGroupTarget(
    target   : GroupTarget
  , nodeGroup: NodeGroup
) extends FullRuleTarget

final case class FullOtherTarget(
    target: NonGroupRuleTarget
) extends FullRuleTarget


final case class FullRuleTargetInfo(
    target     : FullRuleTarget
  , name       : String
  , description: String
  , isEnabled  : Boolean
  , isSystem   : Boolean
) extends HashcodeCaching {

  def toTargetInfo = RuleTargetInfo(
      target = target.target
    , name = name
    , description = description
    , isEnabled = isEnabled
    , isSystem = isSystem
  )
}

