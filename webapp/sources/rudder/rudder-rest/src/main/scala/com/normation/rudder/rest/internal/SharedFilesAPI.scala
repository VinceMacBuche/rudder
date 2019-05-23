/*
***************************o**********************************************************
* Copyright 2016 Normation SwnloaAS
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

package com.normation.rudder.rest.internal

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions

import better.files._

import com.normation.rudder.rest.RestExtractorService
import net.liftweb.common.Box
import net.liftweb.common.EmptyBox
import net.liftweb.common.Failure
import net.liftweb.common.Full
import net.liftweb.common.Loggable
import net.liftweb.http.JsonResponse
import net.liftweb.http.LiftResponse
import net.liftweb.http.Req
import net.liftweb.http.StreamingResponse
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonAST.JArray
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonAST.JValue
import org.joda.time.DateTime

class SharedFilesAPI(
    restExtractor    : RestExtractorService
  , sharedFolderPath : String
) extends RestHelper with Loggable {


  def checkPathAndContinue(path : String, baseFolder : File)(fun : File => Box[LiftResponse]) : Box[LiftResponse] = {
    import net.liftweb.util.Helpers._
    (tryo {
      logger.info("kikoo")
      logger.info(baseFolder.name)
      val filePath = baseFolder /  path.replaceFirst("/","")
      logger.info(filePath.name)
      logger.info(filePath.contains(baseFolder).toString)
      if (baseFolder.contains(filePath, false)) {
        logger.info(filePath)
        fun(filePath)
      } else {
        Failure("Unauthorized access")
      }
    }).flatMap { identity }
  }

  def serialize(file:File) : Box[JValue] = {
    import net.liftweb.json.JsonDSL._
    import net.liftweb.util.Helpers._
    import scala.collection.JavaConverters._
    tryo{
      logger.info(file.lastModifiedTime)
      val date = new DateTime(file.lastModifiedTime.toEpochMilli)
      ( ("name"  -> file.name)
      ~ ("size"  -> file.size)
      ~ ("type"  -> (if (file.isRegularFile) "file" else "dir"))
      ~ ("date"  -> date.toString("yyyy-MM-dd HH:mm:ss"))
      ~ ("rights" -> file.permissionsAsString)
      )
    }
  }

  def errorResponse(message : String): LiftResponse = {
      import net.liftweb.json.JsonDSL._
      val content =
        ( ("success" -> false)
        ~ ("error"   -> message)
        )
      JsonResponse(content,Nil,Nil, 500)
  }

  def downloadFile(file : File) : Box[LiftResponse] = {
    if (file.exists) {
      if (file.isRegularFile) {
        val fileSize = file.size
        val headers =
         ("Content-type" -> "application/octet-stream") ::
         ("Content-length" -> fileSize.toString) ::
         ("Content-disposition" -> s"attachment; filename=${file.name}") ::
         Nil

        Full(StreamingResponse(file.newInputStream,() => {}, fileSize, headers, Nil, 200))
      } else {
        Failure(s"File '${file.name}' is not a regular file")
      }
    } else {
      Failure(s"File '${file.name}' does not exist")
    }
  }

  def directoryContent(directory : File) : Box[LiftResponse] = {
    if (directory.exists) {
      if (directory.isDirectory()) {
        val jsonFiles =  com.normation.utils.Control.sequence(directory.children.toSeq)(serialize)
        jsonFiles.map{files =>
          val result = JObject(List(JField("result",JArray(files.toList))))
          JsonResponse(result,List(),List(), 200)
        }
      } else {
        Failure(s"File '${directory.name}' is not a directory")
      }
    } else {
      Failure(s"File '${directory.name}' does not exist")
    }
  }

  def fileContent(file : File) : Box[LiftResponse] = {
    if (file.exists) {
      if (file.isRegularFile) {
        import net.liftweb.json.JsonDSL._
        import scala.collection.JavaConverters._
        val fileContent : Seq[String] = file.lines(StandardCharsets.UTF_8).toSeq
        val result = JObject(List(JField("result",fileContent.mkString("\n"))))
        Full(JsonResponse(result,List(),List(), 200))
      } else {
        Failure(s"File '${file.name}' is not a regular file")
      }
    } else {
      Failure(s"File '${file.name}' does not exist")
    }

  }
  def requestDispatch(basePath : File) : PartialFunction[Req, () => Box[LiftResponse]] = {

    case Get(Nil, req) => {
      (req.params.get("action") match {
        case None => Failure("'action' is not defined in request")
        case Some("download" :: Nil) =>
          req.params.get("path") match {
            case Some(path :: Nil) =>
              checkPathAndContinue(path, basePath)(downloadFile)
            case None =>
              Failure("Path of file to download is not defined")
            case Some(values) =>
              Failure("Too many values in request for path of file to download")
          }
        case Some(action :: Nil) =>
          Failure("Action not supported")
        case Some(actions) =>
          Failure("Too many values in request for action")
      }) match {
        case Full(response) =>
          response
        case eb : EmptyBox =>
          val fail = eb ?~! s"An error occured while looking into directory"
          logger.error(fail.messageChain)
          errorResponse(fail.messageChain)
      }
    }
    case Post(Nil, req) => {

      (req.json match {
        case Full(json) =>
          json \ "action" match {
            case JString("list") =>
              json \ "path" match {
                case JString(path) =>
                  checkPathAndContinue(path, basePath)(directoryContent)
                case _ => Failure("'path' is not correctly defined for 'list' action")
              }

            case JString("getContent") =>
              json \ "item" match {
                case JString(item) =>
                  checkPathAndContinue(item, basePath)(fileContent)
                case _ => Failure("'item' is not correctly defined for 'getContent' action")
              }

            case _ => Failure("Action not supported")
          }
        case _ => Failure("'action' is not defined in json data")
      }) match {
        case Full(response) =>
          response
        case eb : EmptyBox =>
          val fail = eb ?~! s"An error occured while looking into directory"
          logger.error(fail.messageChain)
          errorResponse(fail.messageChain)
      }

    }
  }


  def ncfRequestDispatch : PartialFunction[Req, () => Box[LiftResponse]] = {


    new PartialFunction[Req, () => Box[LiftResponse]] {
      def isDefinedAt(req: Req): Boolean = {
        logger.info(req.path)
        req.path.partPath match {
          case techniqueId :: techniqueVersion :: "resources" :: _ =>
            val path = File("/var/rudder/configuration-repository/techniques/ncf_techniques/${techniqueId}/${techniqueVersion}/resources")

            val pf = requestDispatch(path)
                         pf.isDefinedAt(req.withNewPath(req.path.drop(3)))

        }
      }

        def apply(req: Req): () => Box[LiftResponse] =
          req.path.partPath match {
            case techniqueId :: techniqueVersion :: "resources" :: _ =>
              val path = File("/var/rudder/configuration-repository/techniques/ncf_techniques/${techniqueId}/${techniqueVersion}/resources")
              path.createIfNotExists(true,true)

              val pf = requestDispatch(path)
              val list = "secure" :: "api" :: "ncf" :: techniqueId :: techniqueVersion :: Nil
            pf.apply(req.withNewPath(req.path.drop(3)))
        }
    }

  }
  serve("secure" :: "api" :: "sharedfile" :: Nil prefix requestDispatch(File(sharedFolderPath)))

  serve("secure" :: "api" :: "ncf" :: Nil prefix ncfRequestDispatch)
}
