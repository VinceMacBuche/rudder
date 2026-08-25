package com.normation.rudder.services.eventlog

import com.normation.errors.IOResult
import com.normation.eventlog.EventLog
import com.normation.eventlog.EventLogRequest
import com.normation.rudder.Rights
import com.normation.rudder.tenants.QueryContext

trait EventLogService {

  /**
   * Get the event logs matching `filter` that `rights` allows to read: an event log is only returned
   * if the caller has at least one of the permissions declared by its type (see `EventLogType.readAuthz`).
   */
  def getUserEventLogs(filter: Option[EventLogRequest], rights: Rights)(implicit qc: QueryContext): IOResult[Seq[EventLog]]

  /**
   * Count the event logs matching `filter` that `rights` allows to read, see `getUserEventLogs`.
   */
  def getUserEventLogCount(filter: Option[EventLogRequest], rights: Rights)(implicit qc: QueryContext): IOResult[Long]

}
