/*
*************************************************************************************
* Copyright 2011-2013 Normation SAS
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

package com.normation.rudder.domain.workflows

import com.normation.rudder.domain.policies.Directive
import com.normation.rudder.domain.policies.DirectiveDiff
import org.joda.time.DateTime
import com.normation.eventlog.EventActor
import com.normation.rudder.domain.policies.SimpleDiff
import com.normation.eventlog.EventActor
import com.normation.rudder.domain.policies.DirectiveId



case class ChangeRequestId(value:String)


sealed trait ChangeRequest {
  def id     : ChangeRequestId //modification Id ?
  def status : ChangeRequestStatusChange
}


///////////////////////////////////////////
///// About the change request status /////
///////////////////////////////////////////


case class ChangeRequestStatus(
    name       : String
  , description: String
  // A marker to know if the ChangeRequest can be modified or not.
  // A change request can be modified only at certain point in
  // workflow, and for example we don't want someone to modify a
  // changeRequest just after the moment a reviewer validated it
  , readOnly   : Boolean
)

sealed trait ChangeRequestStatusDiff

case object AddChangeRequestStatusDiff extends ChangeRequestStatusDiff

case object DeleteChangeRequestStatusDiff extends ChangeRequestStatusDiff

case class ModifyChangeRequestStatusDiff(
    modName       : Option[SimpleDiff[String]]
  , modDescription: Option[SimpleDiff[String]]
) extends ChangeRequestStatusDiff

case class ChangeRequestStatusItem(
    actor       : EventActor
  , creationDate: DateTime
  , reason      : Option[String]
  , diff        : ChangeRequestStatusDiff
)

case class ChangeRequestStatusChange(
    initialState: ChangeRequestStatus
  , firstChange : ChangeRequestStatusDiff
    //the most recent change is in head,
    //the older in tail.
  , nextChanges : Seq[ChangeRequestStatusDiff]
)


////////////////////////////////////////
///// Some types of change request /////
////////////////////////////////////////


case class DirectiveChangeRequest(
    id    : ChangeRequestId //modification Id ?
  , status: ChangeRequestStatusChange
  , change: DirectiveChange
) extends ChangeRequest

case class ConfigurationChangeRequest(
    id        : ChangeRequestId //modification Id ?
  , status    : ChangeRequestStatusChange
  , directives: Map[DirectiveId, DirectiveChange]
  // ... TODO: complete for groups and rules
) extends ChangeRequest


case class RollbackChangeRequest(
    id      : ChangeRequestId //modification Id ?
  , status  : ChangeRequestStatusChange
  , rollback: Null // TODO: rollback change request
) extends ChangeRequest


//////////////////////////////////
///// example for directives /////
//////////////////////////////////


sealed trait ChangeItem[DIFF] {
  def actor       : EventActor
  def creationDate: DateTime
  def reason      : Option[String]
  def diff        : DIFF
}

/**
 * A list of modification on a given item (directive, etc).
 * The parametrisation is as follow:
 * - T is the item type ;
 * - T_DIFF is the type of the eventLog with the diff for
 *   that object.
 * As the class is sealed, you can see implementation example
 * below.
 */
sealed trait Change[T, DIFF, T_CHANGE <: ChangeItem[DIFF]] {
  def initialState: T
  def firstChange : T_CHANGE
  def nextChanges : Seq[T_CHANGE]
}



case class DirectiveChangeItem(
  //no ID: that object does not have any meaning outside
  // a change request
    actor       : EventActor
  , creationDate: DateTime
  , reason      : Option[String]
  , diff        : DirectiveDiff
) extends ChangeItem[DirectiveDiff]


case class DirectiveChange(
    initialState: Directive
  , firstChange : DirectiveChangeItem
    //the most recent change is in head,
    //the older in tail.
  , nextChanges : Seq[DirectiveChangeItem]
) extends Change[Directive, DirectiveDiff, DirectiveChangeItem]
