/*
*************************************************************************************
* Copyright 2016 Normation SAS
*************************************************************************************
*
* This file is part of Rudder.
*
* Rudder is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU General Public License version 3, the copyright holders add
* the following Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU General
* Public License version 3, when you create a Related Module, this
* Related Module is not considered as a part of the work and may be
* distributed under the license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* Rudder is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Rudder.  If not, see <http://www.gnu.org/licenses/>.

*
*************************************************************************************
*/

package com.normation.rudder.web.rest.compliance

import com.normation.rudder.web.rest.RestAPI
import com.normation.rudder.datasources.DataSourceName
import com.normation.rudder.datasources.DataSourceType
import org.joda.time.DateTime
import net.liftweb.common._
import com.normation.rudder.datasources.DataSource
import org.joda.time.Seconds

trait DataSourceApi extends RestAPI {
  val kind = "datasources"
}

case class RestDataSource (
    name : DataSourceName
  , description: Option[String]
  , sourceType : Option[DataSourceType]
  , url        : Option[String]
  , headers    : Option[Map[String,String]]
  , path       : Option[String]
  , frequency  : Option[Seconds]
  , enabled    : Option[Boolean]
) {

  def create() : Box[DataSource] = {

    for {
      sourceType <- Box(sourceType) ?~! "Source type must be defined when creating a data source"
      desc = description.getOrElse("")
      u    = url.getOrElse("")
      p    = path.getOrElse("")
      head = headers.getOrElse(Map())
      freq = frequency.getOrElse(Seconds.seconds(60))
      enab = enabled.getOrElse(false)

    } yield {
      DataSource(
          name
        , desc
        , sourceType
        , u
        , head
        , "get"
        , p
        , freq
        , None
        , enab
      )
    }
  }

  def update(base : DataSource) : DataSource = {

    val sType = sourceType.getOrElse(base.sourceType)
    val desc  = description.getOrElse(base.description)
    val u     = url.getOrElse(base.url)
    val p     = path.getOrElse(base.path)
    val head  = headers.getOrElse(base.headers)
    val freq  = frequency.getOrElse(base.frequency)
    val enab  = enabled.getOrElse(base.enabled)

    base.copy(base.name, desc, sType, u, head, base.httpMethod, p, freq, base.lastUpdate, enab)
  }
}
