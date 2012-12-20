package com.normation.rudder.services.modification

import com.normation.rudder.repository._
import com.normation.eventlog._
import net.liftweb.common._
import com.normation.utils.StringUuidGenerator

class ModificationService(
      eventLogRepository : EventLogRepository
    , gitModificationRepository : GitModificationRepository
    , itemArchiveManager : ItemArchiveManager
    , uuidGen : StringUuidGenerator ) {

  def getCommitsfromEventLog(eventLog:EventLog) : Option[GitCommitId] = {
    eventLog.modificationId match {
      case None =>
      None
      case Some(modId) => gitModificationRepository.getCommits(modId) match {
        case Full(s) => s.headOption
        case eb:EmptyBox => 
          None
      }
    }
  }
  
  def restoreToEventLog(eventLog:EventLog) = {
    getCommitsfromEventLog(eventLog) match {
      case Some(commit) => itemArchiveManager.importAll(commit, ModificationId(uuidGen.newUuid), eventLog.principal, None, false)
      case None => Failure("could not restore eventLog %s".format(eventLog.eventDetails))
    }
  }
  
  def restoreBeforeEventLog(eventLog:EventLog) = {
    getCommitsfromEventLog(eventLog) match {
    case Some(commit) => val parentCommit = GitCommitId(commit.value+"^")
        itemArchiveManager.importAll(parentCommit, ModificationId(uuidGen.newUuid), eventLog.principal, None, false)
      case None => Failure("could not restore before eventLog %s".format(eventLog.eventDetails))
    }
  }
}