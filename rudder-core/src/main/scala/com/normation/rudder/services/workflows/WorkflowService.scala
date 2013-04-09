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

package com.normation.rudder.services.workflows

import scala.collection.mutable.Buffer
import scala.collection.mutable.{ Map => MutMap }
import org.joda.time.DateTime
import com.normation.cfclerk.domain.TechniqueName
import com.normation.eventlog.EventActor
import com.normation.eventlog.ModificationId
import com.normation.rudder.batch.AsyncDeploymentAgent
import com.normation.rudder.batch.AutomaticStartDeployment
import com.normation.rudder.domain.eventlog.RudderEventActor
import com.normation.rudder.domain.nodes.NodeGroupId
import com.normation.rudder.domain.policies._
import com.normation.rudder.domain.workflows._
import com.normation.rudder.repository.RoChangeRequestRepository
import com.normation.rudder.repository.RoDirectiveRepository
import com.normation.rudder.repository.WoDirectiveRepository
import com.normation.rudder.services.policies.DependencyAndDeletionService
import com.normation.utils.Control._
import com.normation.utils.StringUuidGenerator
import net.liftweb.common._
import com.normation.rudder.repository._
import com.normation.rudder.repository.inmemory.InMemoryChangeRequestRepository
import com.normation.rudder.services.eventlog.WorkflowEventLogService


/**
 * That service allows to glue Rudder with the
 * workflows engine.
 * It allows to send new ChangeRequest to the engine,
 * and to be notified when one of them reach the end.
 */
trait WorkflowService {

  /**
   * Start a new workflow process with the given
   * change request or continue an existing
   * wf for that change request
   * (one change request can not have more than
   * one wf at the same time).
   *
   * So for now, a workflow process id IS a changeRequestId.
   * That abstraction is likelly to leak.
   *
   */
  def startWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[WorkflowNodeId]


  def getValidation : Box[Seq[ChangeRequestId]]
  def getDeployment : Box[Seq[ChangeRequestId]]
  def getDeployed   : Box[Seq[ChangeRequestId]]
  def getCancelled  : Box[Seq[ChangeRequestId]]


  val stepsValue :List[WorkflowNodeId]

  val nextSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[WorkflowNodeId])]]
  val backSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[WorkflowNodeId])]]
  def findStep(changeRequestId: ChangeRequestId) : Box[WorkflowNodeId]
}




/**
 * A service that will do whatever is needed to commit
 * modification about a change request in LDAP and
 * deploy them
 */
class CommitAndDeployChangeRequest(
    uuidGen             : StringUuidGenerator
  , roChangeRequestRepo : RoChangeRequestRepository
  , roDirectiveRepo     : RoDirectiveRepository
  , woDirectiveRepo     : WoDirectiveRepository
  , asyncDeploymentAgent: AsyncDeploymentAgent
  , dependencyService   : DependencyAndDeletionService
) extends Loggable {

  def save(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.info(s"Saving and deploying change request ${changeRequestId}")
    for {
      changeRequest <- roChangeRequestRepo.get(changeRequestId)
      saved         <- changeRequest match {
                         case Some(config:ConfigurationChangeRequest) => saveConfigurationChangeRequest(config)
                         case x => Failure("We don't know how to deploy change request like this one: " + x)
                       }
    } yield {
      saved
    }
  }

  /*
   * So, what to do ?
   */
  private[this] def saveConfigurationChangeRequest(cr:ConfigurationChangeRequest) : Box[ChangeRequestId] = {
    import com.normation.utils.Control.sequence

    def doDirectiveChange(directiveChanges:DirectiveChanges, modId: ModificationId) : Box[DirectiveId] = {
      def save(tn:TechniqueName, d:Directive, change: DirectiveChangeItem) = {
        for {
          activeTechnique <- roDirectiveRepo.getActiveTechnique(tn)
          saved           <- woDirectiveRepo.saveDirective(activeTechnique.id, d, modId, change.actor, change.reason)
        } yield {
          saved
        }
      }

      for {
        change <- directiveChanges.changes.change
        done   <- change.diff match {
                    case DeleteDirectiveDiff(tn,d) =>
                      dependencyService.cascadeDeleteDirective(d.id, modId, change.actor, change.reason).map( _ => d.id)
                    case ModifyToDirectiveDiff(tn,d,rs) => save(tn,d, change).map( _ => d.id )
                    case AddDirectiveDiff(tn,d) => save(tn,d, change).map( _ => d.id )
                  }
      } yield {
        done
      }
    }

    def doNodeGroupChange(change:NodeGroupChanges, modId: ModificationId) : Box[NodeGroupId] = {
      val id = change.changes.initialState.map( _.id.value).getOrElse("new group")
      logger.info(s"Save group with id '${id}'")
      Full(NodeGroupId(id))
    }

    val modId = ModificationId(uuidGen.newUuid)

    /*
     * TODO: we should NOT commit into git each modification,
     * but wait until the last line and then commit.
     */

    for {
      directives <- sequence(cr.directives.values.toSeq) { directiveChange =>
                      doDirectiveChange(directiveChange, modId)
                    }
      groups     <- sequence(cr.nodeGroups.values.toSeq) { nodeGroupChange =>
                      doNodeGroupChange(nodeGroupChange, modId)
                    }
    } yield {
      asyncDeploymentAgent ! AutomaticStartDeployment(modId, RudderEventActor)
      cr.id
    }
  }

}
/**
 * The simplest workflow ever, that doesn't wait for approval
 * It has only one state : Deployed
 */
class NoWorkflowServiceImpl(
    commit : CommitAndDeployChangeRequest
  , crRepo : InMemoryChangeRequestRepository
) extends WorkflowService with Loggable {

  val noWorfkflow = WorkflowNodeId("No Workflow")

  val nextSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[WorkflowNodeId])]] = Map()

  val backSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[WorkflowNodeId])]] = Map()

  def findStep(changeRequestId: ChangeRequestId) : Box[WorkflowNodeId] = Failure("No state when no workflow")

  val stepsValue :List[WorkflowNodeId] = List()

  def startWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[WorkflowNodeId] = {
    logger.info("Automatically saving change")
    for {
      result <- commit.save(changeRequestId, actor, reason)
    } yield {
      // always delete the CR
      crRepo.deleteChangeRequest(changeRequestId, actor, reason)
      // and return a no workflow
      noWorfkflow
    }
  }

  // should we keep this one or the previous ??
  def onSuccessWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.info("Automatically saving change")
    for {
      result <- commit.save(changeRequestId, actor, reason)
    } yield {
       // always delete the CR
      crRepo.deleteChangeRequest(changeRequestId, actor, reason)
      // and return a no workflow
      result
    }
  }

  def onFailureWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    // This should never happen, we can't reject in this workflow
    logger.error(s"Change request with ID ${changeRequestId.value} was rejected")
    Failure("Cannot reject a modification when there is no workflow")
  }

  def stepValidationToDeployment(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.error("Invalid use of no workflow : It is impossible to change state when there is no workflow")
    Failure("It is impossible to change state when there is no workflow")
  }


  def stepValidationToDeployed(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.error("Invalid use of no workflow : It is impossible to change state when there is no workflow")
    Failure("It is impossible to change state when there is no workflow")
  }

  def stepValidationToCancelled(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.error("Invalid use of no workflow : It is impossible to change state when there is no workflow")
    Failure("It is impossible to change state when there is no workflow")
  }

  def stepDeploymentToDeployed(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.error("Invalid use of no workflow : It is impossible to change state when there is no workflow")
    Failure("It is impossible to change state when there is no workflow")
  }


  def stepDeploymentToCancelled(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.error("Invalid use of no workflow : It is impossible to change state when there is no workflow")
    Failure("It is impossible to change state when there is no workflow")
  }

  def getValidation : Box[Seq[ChangeRequestId]] = Failure("No state when no workflow")
  def getDeployment : Box[Seq[ChangeRequestId]] = Failure("No state when no workflow")
  def getDeployed   : Box[Seq[ChangeRequestId]] = Failure("No state when no workflow")
  def getCancelled  : Box[Seq[ChangeRequestId]] = Failure("No state when no workflow")

}

class WorkflowServiceImpl(
    workflowLogger : WorkflowEventLogService
  , commit         : CommitAndDeployChangeRequest
  , roWorkflowRepo : RoWorkflowRepository
  , woWorkflowRepo : WoWorkflowRepository
) extends WorkflowService with Loggable {

  private[this] case object Validation extends WorkflowNode {
    val id = WorkflowNodeId("Pending validation")
  }

  private[this] case object Deployment extends WorkflowNode {
    val id = WorkflowNodeId("Pending deployment")
  }

  private[this] case object Deployed extends WorkflowNode {
    val id = WorkflowNodeId("Deployed")
  }

  private[this] case object Cancelled extends WorkflowNode {
    val id = WorkflowNodeId("Cancelled")
  }

  private[this] val steps:List[WorkflowNode] = List(Validation,Deployment,Deployed,Cancelled)

  val stepsValue = steps.map(_.id)

  val nextSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[WorkflowNodeId])]] =
    steps.map{
      case Validation => Validation.id -> Seq((Deployment.id,stepValidationToDeployment _),(Deployed.id,stepValidationToDeployed _))
      case Deployment => Deployment.id -> Seq((Deployed.id,stepDeploymentToDeployed _))
      case Deployed   => Deployed.id -> Seq()
      case Cancelled  => Cancelled.id -> Seq()
    }.toMap

  val backSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[WorkflowNodeId])]] =
    steps.map{
      case Validation => Validation.id -> Seq((Cancelled.id,stepValidationToCancelled _))
      case Deployment => Deployment.id -> Seq((Cancelled.id,stepDeploymentToCancelled _))
      case Deployed   => Deployed.id -> Seq()
      case Cancelled  => Cancelled.id -> Seq()
    }.toMap

  def findStep(changeRequestId: ChangeRequestId) : Box[WorkflowNodeId] = {
    roWorkflowRepo.getStateOfChangeRequest(changeRequestId)
  }

  private[this] def changeStep(
      from           : WorkflowNode
    , to             : WorkflowNode
    , changeRequestId: ChangeRequestId
    , actor          : EventActor
    , reason         : Option[String]
  ) : Box[WorkflowNodeId] = {
    (for {
      state <- woWorkflowRepo.updateState(changeRequestId,from.id, to.id)
      workflowStep = WorkflowStepChange(changeRequestId,from.id,to.id)
      log   <- workflowLogger.saveEventLog(workflowStep,actor,reason)
    } yield {
      state
    }) match {
      case Full(state) => Full(state)
      case e:Failure => logger.error(s"Error when changing step in workflow for Change Request ${changeRequestId.value} : ${e.msg}")
                        e
      case Empty => logger.error(s"Error when changing step in workflow for Change Request ${changeRequestId.value} : no reason given")
                    Empty
    }
  }

  private[this] def toFailure(from: WorkflowNode, changeRequestId: ChangeRequestId, actor: EventActor, reason: Option[String]) : Box[WorkflowNodeId] = {
    changeStep(from, Cancelled,changeRequestId,actor,reason)
  }

  def startWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[WorkflowNodeId] = {
    logger.debug("start workflow")
    woWorkflowRepo.createWorkflow(changeRequestId, Validation.id)
  }

  private[this]  def onSuccessWorkflow(from: WorkflowNode, changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[WorkflowNodeId] = {
    logger.debug("update")
    for {
      save  <- commit.save(changeRequestId, actor, reason)
      state <- changeStep(from,Deployed,changeRequestId,actor,reason)
    } yield {
      state
    }

  }

  //allowed workflow steps


  private[this] def stepValidationToDeployment(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[WorkflowNodeId] = {
    changeStep(Validation, Deployment,changeRequestId, actor, reason)
  }


  private[this] def stepValidationToDeployed(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[WorkflowNodeId] = {
    onSuccessWorkflow(Validation, changeRequestId, actor, reason)
  }

  private[this] def stepValidationToCancelled(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[WorkflowNodeId] = {
    toFailure(Validation, changeRequestId, actor, reason)
  }

  private[this] def stepDeploymentToDeployed(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[WorkflowNodeId] = {
    onSuccessWorkflow(Deployment, changeRequestId, actor, reason)
  }


  private[this] def stepDeploymentToCancelled(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[WorkflowNodeId] = {
    toFailure(Deployment, changeRequestId, actor, reason)
  }


  def getValidation : Box[Seq[ChangeRequestId]] = roWorkflowRepo.getAllByState(Validation.id)
  def getDeployment : Box[Seq[ChangeRequestId]] = roWorkflowRepo.getAllByState(Deployment.id)
  def getDeployed   : Box[Seq[ChangeRequestId]] = roWorkflowRepo.getAllByState(Deployed.id)
  def getCancelled  : Box[Seq[ChangeRequestId]] = roWorkflowRepo.getAllByState(Cancelled.id)
}
