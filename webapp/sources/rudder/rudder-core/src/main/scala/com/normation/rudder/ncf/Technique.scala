/*
*************************************************************************************
* Copyright 2017 Normation SAS
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

package com.normation.rudder.ncf

import better.files.File
import com.normation.inventory.domain.Version
import com.normation.inventory.domain.AgentType
import com.normation.rudder.ncf.Technique.baseNcfTechniquePath
import com.normation.rudder.ncf.Technique.savedNcfTechniquePath

sealed trait NcfId {
  def value : String
  def validDscName : String
}
final case class BundleName (value : String) extends NcfId {
  val validDscName : String = value.split("_").map(_.capitalize).mkString("-")
}

final case class ParameterId (value : String) extends NcfId  {
  val validDscName : String = value.split("_").map(_.capitalize).mkString("")
}

object  ResourceFile {
  sealed trait ResourceFileState
  case object New       extends ResourceFileState
  case object Deleted   extends ResourceFileState
  case object Modified  extends ResourceFileState
  case object Unchanged extends ResourceFileState

  def savedResourceFolder(tech : Technique) = savedNcfTechniquePath / tech.bundleName.value / tech.version.value / "resources"
  def baseResourceFolder(tech : Technique) = baseNcfTechniquePath / tech.bundleName.value / tech.version.value / "resources"
}

case class ResourceFile(
    path  : String
  , state : ResourceFile.ResourceFileState
) {
  val file = File(path)
}

final case class Technique(
    bundleName  : BundleName
  , name        : String
  , methodCalls : Seq[MethodCall]
  , version     : Version
  , description : String
  , parameters  : Seq[TechniqueParameter]
  , ressources  : Seq[ResourceFile]
)

object Technique {
  val baseNcfTechniquePath = File("/var/rudder/configuration-repository/ncf")
  val savedNcfTechniquePath = File("/var/rudder/configuration-repository/techniques/ncf_techniques")
}

final case class MethodCall(
    methodId   : BundleName
  , parameters : Map[ParameterId,String]
  , condition  : String
  , component  : String
)

final case class GenericMethod(
    id             : BundleName
  , name           : String
  , parameters     : Seq[MethodParameter]
  , classParameter : ParameterId
  , classPrefix    : String
  , agentSupport   : Seq[AgentType]
  , description    : String
)

final case class MethodParameter(
    id          : ParameterId
  , description : String
)

final case class TechniqueParameter (
    id   : ParameterId
  , name : ParameterId
)