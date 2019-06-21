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

import java.util.regex.Pattern

import better.files.File
import cats.data.NonEmptyList
import com.normation.inventory.domain.Version
import com.normation.inventory.domain.AgentType
import com.normation.rudder.ncf.Constraint.Constraint
import com.normation.rudder.ncf.Constraint.CheckResult
import com.normation.rudder.ncf.Constraint.NOK
import com.normation.rudder.ncf.Constraint.OK

import scala.util.matching.Regex

sealed trait NcfId {
  def value : String
  def validDscName : String
  def canonify : String = value.replaceAll("[^a-zA-Z0-9_]","_")
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
  , constraint  : List[Constraint]
)
/*
final case class Constraint(
  allowEmpty      : Boolean
, allowWhiteSpace : Boolean
, maxLength       : Int
, minLength       : Option[Int]
, regex           : Option[String]
, notRegex        : Option[String]
, select          : Option[List[String]]
)
*/
final case class TechniqueParameter (
    id   : ParameterId
  , name : ParameterId
)

object Constraint {
  sealed trait Constraint {
    def check (value : String) : CheckResult
  }
  import NonEmptyList.one

  case object NonEmpty extends  Constraint {
    def check(value: String): CheckResult = {
      if (value.nonEmpty) {
        OK
      } else {
        NOK(one("Must not be empty"))
      }
    }
  }

  case object NoWhiteSpace extends  Constraint {
    def check(value: String): CheckResult = {
      if (value.matches("""^\S.*\S$""")) {
        OK
      } else {
        NOK(one("Must not have leading or trailing whitespaces "))
      }
    }
  }
  case class MaxLength(max : Int) extends Constraint {
    def check(value: String): CheckResult = {
      if (value.size <= max) {
        OK
      } else {
        val agentMaxNotice = if (max == 16384) " Fields over 16384 characters are currently not supported. If you want to edit a file, please insert your content into a file, and copy it with a file_copy_* method, or use a template." else ""
        NOK(one(s"Max length is ${max}. Current size is ${value.size}.${agentMaxNotice}"))
      }
    }
  }
  case class MinLength(min : Int) extends Constraint {
    def check(value: String): CheckResult = {
      if (value.size >= min) {
        OK
      } else {
        NOK(one(s"Min length is ${min}. Current size is ${value.size}."))
      }
    }
  }
  case class MatchRegex(regex : String) extends Constraint {
    def check(value: String): CheckResult = {
      if (value.matches(regex)) {
        OK
      } else {
        NOK(one(s"Must match regex '${regex}'"))
      }
    }
  }
  case class NotMatchRegex(regex : String) extends Constraint {
    def check(value: String): CheckResult = {
      if (!value.matches(regex)) {
        OK
      } else {
        NOK(one(s"Must not match regex '${regex}'"))
      }
    }
  }
  case class FromList(list : List[String]) extends Constraint {
    def check(value: String): CheckResult = {
      if (list.contains(value)) {
        OK
      } else {
        NOK(one(s"Must be an accepted value: ${list.mkString(", ")}"))
      }
    }
  }
  sealed trait CheckResult
  case object OK extends CheckResult
  case class NOK (cause : NonEmptyList[String]) extends CheckResult
}


object CheckConstraint  {
  def check(constraint: List[Constraint.Constraint], value : String) : CheckResult = {
    import Constraint._

    (constraint.map(_.check(value)) :\ (OK : CheckResult)) {
      case (OK, OK) => OK
      case (NOK(m1), NOK(m2)) => NOK(m1 ::: m2)
      case (res:NOK,_) => res
      case (_,res:NOK) => res

    }
  }
}