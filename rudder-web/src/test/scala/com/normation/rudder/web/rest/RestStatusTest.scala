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

package com.normation.rudder.web.rest

import org.junit._
import org.junit.Assert._
import org.junit.runner._
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._

import RestTestSetUp._
import net.liftweb.common.Full
import net.liftweb.http.PlainTextResponse
import net.liftweb.common.Loggable
import net.liftweb.http.JsonResponse
import net.liftweb.http.LiftResponse
import net.liftweb.common.Failure
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JArray
import net.liftweb.json.JValue
import com.normation.rudder.datasources.DataSource
import org.joda.time.Seconds
import com.normation.rudder.datasources.DataSourceName
import net.liftweb.common.Box
import com.normation.rudder.web.rest.compliance.DataSourceApi

@RunWith(classOf[JUnitRunner])
class RestStatusTest extends Specification with Loggable {

  "testing status REST API" should {
    "be correct" in {
      testGET("/api/status") { req =>
       RestStatus(req)() must beEqualTo(Full(PlainTextResponse("OK")))
      }
    }
  }
}
